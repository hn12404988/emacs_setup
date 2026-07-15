#!/usr/bin/env python3
"""Red tests for /stt endpoint (issue hn12404988/emacs_setup#7).

POST /stt accepts chunked 16kHz mono i16 PCM via Transfer-Encoding: chunked,
feeds each chunk to sherpa-onnx OnlineRecognizer with RKNN backend, and
returns {"text": "..."} on end-of-stream (zero-length chunk).

These tests are RED by design — the /stt endpoint does not exist yet.
"""

import io
import json
import struct
import sys
import threading
import unittest
from http.server import HTTPServer
from unittest.mock import MagicMock


# ── helpers ────────────────────────────────────────────────────────────────

def _pcm_chunk(samples: list[int]) -> bytes:
    """Pack a list of i16 samples into a little-endian PCM byte string."""
    return struct.pack(f"<{len(samples)}h", *samples)


SILENCE_1S = _pcm_chunk([0] * 16000)     # 1 second of 16kHz mono silence
SILENCE_100MS = _pcm_chunk([0] * 1600)    # 100ms of silence
TONE_440HZ_100MS = _pcm_chunk(            # 100ms of a 440 Hz sine at 16kHz
    [int(16000 * __import__("math").sin(2 * 3.1415926535 * 440 * i / 16000))
     for i in range(1600)]
)


# ── mock sherpa-onnx ───────────────────────────────────────────────────────
#
# The real sherpa_onnx module is not available on this machine (it requires
# a Rockchip NPU).  We inject a minimal fake so the test file can validate
# the HTTP / integration layer without hardware.

class _MockRecognizer:
    """Fake OnlineRecognizer that records fed samples and returns canned text."""

    def __init__(self, *args, **kwargs):
        self.samples_fed: list[bytes] = []
        self._stream = None

    def create_stream(self):
        self._stream = _MockStream(self)
        return self._stream

    def decode_stream(self, stream):
        return ""

    def is_ready(self, stream):
        return False

    def get_result(self, stream):
        return "mock transcription"


class _MockStream:
    def __init__(self, recognizer: _MockRecognizer):
        self._recognizer = recognizer

    def accept_waveform(self, sample_rate: int, samples: bytes):
        self._recognizer.samples_fed.append(samples)


_MOCK_SHERPA_ONNX = MagicMock()
_MOCK_SHERPA_ONNX.OnlineRecognizer = _MockRecognizer


# ── mock ctypes (librkllmrt.so is hardware-specific) ───────────────────────

class _MockCDLL:
    """Fake CDLL that doesn't try to dlopen a real .so."""

    def __init__(self, path, *args, **kwargs):
        self._path = path

    def __getattr__(self, name):
        def _noop(*args, **kwargs):
            return 0
        return _noop


class _MockCType:
    """A minimal ctypes-like type that supports array-multiplication syntax
    (e.g. ``c_uint8 * 112``) and can be used as a field type in Structure."""

    def __mul__(self, n: int):
        return _MockCType()

    def __rmul__(self, n: int):
        return _MockCType()

    def __call__(self, *args, **kwargs):
        return None


class _MockStructureMeta(type):
    """Metaclass so ``class Foo(ctypes.Structure): _fields_ = [...]`` works."""

    def __new__(mcs, name, bases, ns):
        return super().__new__(mcs, name, bases, ns)


class _MockStructure(metaclass=_MockStructureMeta):
    pass


class _MockUnion(metaclass=_MockStructureMeta):
    pass


_MOCK_CTYPES = MagicMock()
_MOCK_CTYPES.c_void_p = _MockCType()
for _typ in ("c_int32", "c_int", "c_char_p", "c_float", "c_bool", "c_size_t",
             "c_uint8", "c_int8"):
    setattr(_MOCK_CTYPES, _typ, _MockCType())
_MOCK_CTYPES.POINTER = lambda t: _MockCType()
_MOCK_CTYPES.Structure = _MockStructure
_MOCK_CTYPES.Union = _MockUnion
_MOCK_CTYPES.CDLL = _MockCDLL
_MOCK_CTYPES.CFUNCTYPE = staticmethod(lambda *a, **kw: (lambda fn: fn))
_MOCK_CTYPES.byref = lambda x: x


# ── import server under mocks ──────────────────────────────────────────────

_orig_ctypes = sys.modules.get("ctypes")
_orig_sherpa = sys.modules.get("sherpa_onnx")
sys.modules["ctypes"] = _MOCK_CTYPES
sys.modules["sherpa_onnx"] = _MOCK_SHERPA_ONNX

_LOCAL_LLM_DIR = __import__("pathlib").Path(__file__).resolve().parent
if str(_LOCAL_LLM_DIR) not in sys.path:
    sys.path.insert(0, str(_LOCAL_LLM_DIR))

import server as server_mod  # type: ignore[import-not-found]

# Restore originals
if _orig_ctypes is not None:
    sys.modules["ctypes"] = _orig_ctypes
else:
    del sys.modules["ctypes"]
if _orig_sherpa is not None:
    sys.modules["sherpa_onnx"] = _orig_sherpa
else:
    del sys.modules["sherpa_onnx"]


# ── test base class ────────────────────────────────────────────────────────

class _STTTestBase(unittest.TestCase):
    """Base test that starts a real HTTP server on a random port."""

    @classmethod
    def setUpClass(cls):
        cls._server = HTTPServer(("127.0.0.1", 0), server_mod._Handler)
        cls.port = cls._server.server_address[1]
        cls._thread = threading.Thread(
            target=cls._server.serve_forever, daemon=True
        )
        cls._thread.start()

    @classmethod
    def tearDownClass(cls):
        cls._server.shutdown()
        cls._server.server_close()

    def _chunked_post(self, path: str, chunks: list[bytes],
                      content_type: str = "audio/l16; rate=16000; channels=1",
                      extra_headers: dict | None = None,
                      ) -> tuple[int, dict]:
        """Send a chunked POST request; return (status_code, json_body)."""
        import http.client

        conn = http.client.HTTPConnection("127.0.0.1", self.port, timeout=5)
        headers = {
            "Transfer-Encoding": "chunked",
            "Content-Type": content_type,
            **(extra_headers or {}),
        }
        # Build the chunked body manually
        body = b""
        for chunk in chunks:
            body += f"{len(chunk):X}\r\n".encode() + chunk + b"\r\n"
        body += b"0\r\n\r\n"  # terminating chunk

        conn.request("POST", path, body=body, headers=headers)
        resp = conn.getresponse()
        data = resp.read()
        conn.close()
        return resp.status, json.loads(data) if data else {}


# ── RED tests ──────────────────────────────────────────────────────────────

class STTEndpointRoutingTests(_STTTestBase):
    """Verify the /stt route exists and rejects invalid requests."""

    def test_01_stt_endpoint_exists(self):
        """POST /stt with valid chunked PCM must return 200, not 404."""
        status, body = self._chunked_post("/stt", [SILENCE_100MS])

        self.assertNotEqual(status, 404,
            "POST /stt returns 404 — endpoint must be registered in do_POST")
        self.assertEqual(status, 200,
            f"Expected 200 OK, got {status}: {body}")

    def test_02_stt_rejects_wrong_content_type(self):
        """POST /stt with application/json must return 415 Unsupported Media Type."""
        status, body = self._chunked_post(
            "/stt", [], content_type="application/json")
        # Currently returns 404 (not 415) — test is red.
        self.assertEqual(status, 415,
            f"Expected 415 for application/json, got {status}: {body}")

    def test_03_stt_rejects_empty_content_type(self):
        """POST /stt without Content-Type must return 400."""
        status, body = self._chunked_post("/stt", [], content_type="")
        # Currently returns 404 — test is red.
        self.assertEqual(status, 400,
            f"Expected 400 for missing Content-Type, got {status}: {body}")


class STTChunkedStreamingTests(_STTTestBase):
    """Integration tests for chunked PCM streaming to /stt."""

    def test_04_zero_chunk_triggers_end_of_stream(self):
        """Sending only a terminating (zero-length) chunk must return
        {"text": "..."} with status 200."""
        status, body = self._chunked_post("/stt", [])

        self.assertEqual(status, 200,
            f"Expected 200 after zero-length chunk (EOS), got {status}: {body}")
        self.assertIn("text", body,
            f"Response must contain 'text' key: {body}")
        self.assertIsInstance(body.get("text"), str,
            f"'text' must be a string, got {type(body.get('text'))}")

    def test_05_nonempty_chunks_dont_respond_until_eos(self):
        """A single non-empty audio chunk followed by the terminating chunk
        must produce a valid {"text": "..."} response."""
        status, body = self._chunked_post("/stt", [SILENCE_100MS])

        self.assertEqual(status, 200,
            f"Expected 200 after full chunked stream, got {status}: {body}")
        self.assertIn("text", body,
            "Response must include 'text' key at end-of-stream")

    def test_06_multiple_chunks_are_streamed(self):
        """Multiple PCM chunks over a single chunked request must be accepted."""
        chunks = [SILENCE_100MS, TONE_440HZ_100MS, SILENCE_100MS]
        status, body = self._chunked_post("/stt", chunks)

        self.assertEqual(status, 200,
            f"Expected 200 for multi-chunk stream, got {status}: {body}")
        self.assertIn("text", body,
            f"Response after multi-chunk stream must have 'text': {body}")

    def test_07_invalid_pcm_sample_rate_is_rejected(self):
        """Content-Type with an unsupported sample rate (8kHz) must return 400."""
        status, body = self._chunked_post(
            "/stt", [SILENCE_100MS],
            content_type="audio/l16; rate=8000; channels=1",
        )
        self.assertEqual(status, 400,
            f"Expected 400 for 8kHz PCM, got {status}: {body}")

    def test_08_stereo_pcm_is_rejected(self):
        """Stereo (2-channel) PCM must be rejected with 400."""
        status, body = self._chunked_post(
            "/stt", [SILENCE_100MS],
            content_type="audio/l16; rate=16000; channels=2",
        )
        self.assertEqual(status, 400,
            f"Expected 400 for stereo PCM, got {status}: {body}")

    def test_09_missing_content_type_is_rejected(self):
        """POST /stt with empty Content-Type must return 400."""
        status, body = self._chunked_post("/stt", [], content_type="")

        self.assertEqual(status, 400,
            f"Expected 400 for empty Content-Type, got {status}: {body}")
