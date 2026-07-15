#!/usr/bin/env python3
"""rkllm shim — translates a small HTTP API to the Rockchip rkllm runtime.

Endpoints
  POST /infill   (FIM code completion, used by Emacs inline-suggestion)
    request:  {"input_prefix": str, "input_suffix": str, "n_predict": int}
    response: {"content": str}

  POST /commit   (git commit-message generation, used by Emacs C-x c)
    request:  {"diff": str, "max_tokens": int (optional)}
    response: {"message": str}

Models are loaded lazily and freed after an idle TTL (see ModelSpec / _reaper),
so an idle daemon holds ~no model memory. Two models are served: a small base
FIM model for /infill and a larger Coder-Instruct model for /commit. The RK3588
NPU holds only ONE model at a time (loading a second model while one is resident
fails to allocate NPU memory and crashes), so this is a single-slot cache:
loading a model first frees any other. A single lock serializes load / run /
destroy, and the NPU runs one inference at a time.

Pinned to rkllm runtime v1.1.4 — struct layouts and the ABI here track that
release. (The v1.1.2 and v1.1.4 rkllm.h headers are byte-identical, so the
ctypes layout below is unchanged from the 1.1.2 era.) Upgrading librkllmrt.so
again requires re-checking rkllm.h.
"""

import ctypes
import json
import re
import sherpa_onnx
import sys
import threading
import time
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path

# Qwen special-token markers that occasionally leak through skip_special_token
# (notably the FIM ones). Stripped post-decode as a safety net.
_SPECIAL_TOKEN_RE = re.compile(
    r"<\|(?:fim_(?:prefix|suffix|middle|pad)"
    r"|repo_name|file_sep|endoftext|im_(?:start|end))\|>"
)

HERE = Path(__file__).resolve().parent
LIB_PATH = HERE / "lib" / "librkllmrt.so"
MODELS = HERE / "models"

HOST, PORT = "127.0.0.1", 8080

# --- FIM model (fast, base Qwen2.5-Coder-1.5B) -----------------------------
FIM_MODEL_PATH = MODELS / "Qwen2.5-Coder-1.5B-rk3588-w8a8-opt-1-hybrid-ratio-0.0.rkllm"
FIM_CONTEXT_LEN = 2048
FIM_MAX_NEW_TOKENS = 100          # hard cap per request
FIM_TTL = 30 * 60                 # evict after 30 min idle (keeps warm in a session)
# Qwen2.5-Coder ids that should halt FIM generation:
#   151643 <|endoftext|>, 151645 <|im_end|>, 151662 <|fim_pad|>, 151664 <|file_sep|>
FIM_STOP_IDS = frozenset({151643, 151645, 151662, 151664})

# --- Commit model (Qwen2.5-Coder-3B-Instruct, instruction-tuned) -----------
COMMIT_MODEL_PATH = MODELS / "Qwen2.5-Coder-3B-Instruct.rkllm"
COMMIT_CONTEXT_LEN = 4096
COMMIT_MAX_NEW_TOKENS = 96        # a commit subject is short; this is the ceiling
COMMIT_TTL = 10 * 60              # evict after 10 min idle (commits are infrequent)
MAX_DIFF_CHARS = 6000             # trim large diffs to protect prefill / latency
# Chat-mode stop ids: 151643 <|endoftext|>, 151645 <|im_end|>
CHAT_STOP_IDS = frozenset({151643, 151645})

# Hardcoded system prompt for commit-message generation. Edit this to tune the
# style of generated messages — it is the single place that controls them.
COMMIT_SYSTEM_PROMPT = (
    "You are a tool that writes git commit messages. "
    "You are given a git diff of the staged changes. "
    "Write ONE concise commit subject line that summarizes what the change does. "
    "Rules: use the imperative mood (e.g. 'Add', 'Fix', 'Refactor'); "
    "keep it under ~70 characters; no trailing period; "
    "use a Conventional Commits prefix (feat:, fix:, refactor:, docs:, chore:, "
    "test:, perf:) when it clearly fits. "
    "Write the WHOLE message on a SINGLE line — no line breaks, no body. "
    "The prefix and the description go on the same line, e.g. "
    "'fix: handle empty input' (never the prefix alone on its own line). "
    "Output ONLY the commit message line — no quotes, no explanation, no code fences."
)

REAPER_INTERVAL = 60              # seconds between idle-eviction sweeps


# ---------------------------------------------------------------------------
# ctypes mappings for rkllm v1.1.4 (identical layout to v1.1.2)
# ---------------------------------------------------------------------------

LLMHandle = ctypes.c_void_p


class RKLLMExtendParam(ctypes.Structure):
    _fields_ = [
        ("base_domain_id", ctypes.c_int32),
        ("reserved", ctypes.c_uint8 * 112),
    ]


class RKLLMParam(ctypes.Structure):
    _fields_ = [
        ("model_path", ctypes.c_char_p),
        ("max_context_len", ctypes.c_int32),
        ("max_new_tokens", ctypes.c_int32),
        ("top_k", ctypes.c_int32),
        ("top_p", ctypes.c_float),
        ("temperature", ctypes.c_float),
        ("repeat_penalty", ctypes.c_float),
        ("frequency_penalty", ctypes.c_float),
        ("presence_penalty", ctypes.c_float),
        ("mirostat", ctypes.c_int32),
        ("mirostat_tau", ctypes.c_float),
        ("mirostat_eta", ctypes.c_float),
        ("skip_special_token", ctypes.c_bool),
        ("is_async", ctypes.c_bool),
        ("img_start", ctypes.c_char_p),
        ("img_end", ctypes.c_char_p),
        ("img_content", ctypes.c_char_p),
        ("extend_param", RKLLMExtendParam),
    ]


class RKLLMEmbedInput(ctypes.Structure):
    _fields_ = [("embed", ctypes.POINTER(ctypes.c_float)), ("n_tokens", ctypes.c_size_t)]


class RKLLMTokenInput(ctypes.Structure):
    _fields_ = [("input_ids", ctypes.POINTER(ctypes.c_int32)), ("n_tokens", ctypes.c_size_t)]


class RKLLMMultiModelInput(ctypes.Structure):
    _fields_ = [
        ("prompt", ctypes.c_char_p),
        ("image_embed", ctypes.POINTER(ctypes.c_float)),
        ("n_image_tokens", ctypes.c_size_t),
    ]


class _RKLLMInputUnion(ctypes.Union):
    _fields_ = [
        ("prompt_input", ctypes.c_char_p),
        ("embed_input", RKLLMEmbedInput),
        ("token_input", RKLLMTokenInput),
        ("multimodal_input", RKLLMMultiModelInput),
    ]


class RKLLMInput(ctypes.Structure):
    _anonymous_ = ("u",)
    _fields_ = [
        ("input_type", ctypes.c_int),
        ("u", _RKLLMInputUnion),
    ]


class RKLLMLoraParam(ctypes.Structure):
    _fields_ = [("lora_adapter_name", ctypes.c_char_p)]


class RKLLMPromptCacheParam(ctypes.Structure):
    _fields_ = [("save_prompt_cache", ctypes.c_int), ("prompt_cache_path", ctypes.c_char_p)]


class RKLLMInferParam(ctypes.Structure):
    _fields_ = [
        ("mode", ctypes.c_int),
        ("lora_params", ctypes.POINTER(RKLLMLoraParam)),
        ("prompt_cache_params", ctypes.POINTER(RKLLMPromptCacheParam)),
    ]


class RKLLMResultLastHiddenLayer(ctypes.Structure):
    _fields_ = [
        ("hidden_states", ctypes.POINTER(ctypes.c_float)),
        ("embd_size", ctypes.c_int),
        ("num_tokens", ctypes.c_int),
    ]


class RKLLMResult(ctypes.Structure):
    _fields_ = [
        ("text", ctypes.c_char_p),
        ("token_id", ctypes.c_int32),
        ("last_hidden_layer", RKLLMResultLastHiddenLayer),
    ]


LLMResultCallback = ctypes.CFUNCTYPE(
    None, ctypes.POINTER(RKLLMResult), ctypes.c_void_p, ctypes.c_int
)

# Enums
RKLLM_INPUT_PROMPT = 0
RKLLM_INFER_GENERATE = 0
RKLLM_RUN_NORMAL = 0
RKLLM_RUN_WAITING = 1
RKLLM_RUN_FINISH = 2
RKLLM_RUN_ERROR = 3

rkllm = ctypes.CDLL(str(LIB_PATH))
rkllm.rkllm_createDefaultParam.argtypes = []
rkllm.rkllm_createDefaultParam.restype = RKLLMParam
rkllm.rkllm_init.argtypes = [
    ctypes.POINTER(LLMHandle), ctypes.POINTER(RKLLMParam), LLMResultCallback,
]
rkllm.rkllm_init.restype = ctypes.c_int
rkllm.rkllm_run.argtypes = [
    LLMHandle, ctypes.POINTER(RKLLMInput), ctypes.POINTER(RKLLMInferParam), ctypes.c_void_p,
]
rkllm.rkllm_run.restype = ctypes.c_int
rkllm.rkllm_destroy.argtypes = [LLMHandle]
rkllm.rkllm_destroy.restype = ctypes.c_int
rkllm.rkllm_abort.argtypes = [LLMHandle]
rkllm.rkllm_abort.restype = ctypes.c_int


# ---------------------------------------------------------------------------
# Inference state — one inference at a time, serialized by _lock
# ---------------------------------------------------------------------------

_lock = threading.Lock()          # serializes load / run / destroy across models
_chunks: list[str] = []           # output token strings for the in-flight run
_aborted = False
_token_count = 0
_max_tokens = 0                   # per-request abort cap, set in _generate
_active_handle = LLMHandle()      # handle of the currently-running model (for abort)
_active_stop_ids = frozenset()    # stop-token ids for the currently-running model


@LLMResultCallback
def _on_result(result_ptr, _userdata, state):
    global _aborted, _token_count
    if state == RKLLM_RUN_WAITING:
        return
    if state in (RKLLM_RUN_FINISH, RKLLM_RUN_ERROR):
        return
    res = result_ptr.contents
    if res.text:
        _chunks.append(res.text.decode("utf-8", errors="replace"))
    _token_count += 1
    hit_cap = _token_count >= _max_tokens
    hit_stop = res.token_id in _active_stop_ids
    if (hit_cap or hit_stop) and not _aborted:
        _aborted = True
        rkllm.rkllm_abort(_active_handle)


# ---------------------------------------------------------------------------
# Model manager — lazy load, idle-TTL eviction
# ---------------------------------------------------------------------------

class ModelSpec:
    """A model the server can serve, loaded on demand and freed when idle."""

    def __init__(self, name, model_path, max_context_len, max_new_tokens, stop_ids, ttl):
        self.name = name
        self.model_path = model_path
        self.max_context_len = max_context_len
        self.max_new_tokens = max_new_tokens
        self.stop_ids = stop_ids
        self.ttl = ttl
        self.handle = None            # LLMHandle once loaded, else None
        self.last_used = 0.0

    def load(self):
        """rkllm_init this model. Caller must hold _lock."""
        if not self.model_path.exists():
            raise RuntimeError(f"model missing at {self.model_path}")
        handle = LLMHandle()
        param = rkllm.rkllm_createDefaultParam()
        param.model_path = str(self.model_path).encode()
        param.max_context_len = self.max_context_len
        param.max_new_tokens = self.max_new_tokens
        param.top_k = 1
        param.top_p = 0.9
        param.temperature = 0.0
        param.repeat_penalty = 1.1
        param.skip_special_token = True
        param.is_async = False
        t0 = time.time()
        rc = rkllm.rkllm_init(ctypes.byref(handle), ctypes.byref(param), _on_result)
        if rc != 0:
            raise RuntimeError(f"rkllm_init failed for {self.name} (rc={rc})")
        self.handle = handle
        print(f"[rkllm] loaded {self.name} ({self.model_path.name}) "
              f"in {time.time() - t0:.1f}s", flush=True)

    def unload(self, reason: str = "idle"):
        """rkllm_destroy this model. Caller must hold _lock."""
        if self.handle is not None:
            rkllm.rkllm_destroy(self.handle)
            self.handle = None
            print(f"[rkllm] freed {self.name} ({reason})", flush=True)


_SPECS = {
    "fim": ModelSpec("fim", FIM_MODEL_PATH, FIM_CONTEXT_LEN, FIM_MAX_NEW_TOKENS,
                     FIM_STOP_IDS, FIM_TTL),
    "commit": ModelSpec("commit", COMMIT_MODEL_PATH, COMMIT_CONTEXT_LEN,
                        COMMIT_MAX_NEW_TOKENS, CHAT_STOP_IDS, COMMIT_TTL),
}


def _generate(name: str, prompt: str, max_tokens: int) -> str:
    """Run one inference on model NAME, loading it first if needed.

    Serialized by _lock so the shared callback globals are safe and the NPU
    only runs one model at a time. Special tokens are stripped from the output.
    """
    global _chunks, _aborted, _token_count, _max_tokens, _active_handle, _active_stop_ids
    spec = _SPECS[name]
    with _lock:
        if spec.handle is None:
            # The RK3588 NPU holds only ONE model at a time: loading a second
            # model while another is resident fails to allocate NPU memory and
            # segfaults. So free any other loaded model before loading this one.
            for other in _SPECS.values():
                if other is not spec and other.handle is not None:
                    other.unload(f"swap -> {spec.name}")
            spec.load()
        _chunks = []
        _aborted = False
        _token_count = 0
        _max_tokens = max(1, min(max_tokens, spec.max_new_tokens))
        _active_handle = spec.handle
        _active_stop_ids = spec.stop_ids
        rkin = RKLLMInput()
        rkin.input_type = RKLLM_INPUT_PROMPT
        rkin.prompt_input = prompt.encode("utf-8")
        infer = RKLLMInferParam()
        infer.mode = RKLLM_INFER_GENERATE
        infer.lora_params = None
        infer.prompt_cache_params = None
        rc = rkllm.rkllm_run(spec.handle, ctypes.byref(rkin), ctypes.byref(infer), None)
        if rc != 0 and not _aborted:
            raise RuntimeError(f"rkllm_run failed (rc={rc})")
        spec.last_used = time.time()
        return _SPECIAL_TOKEN_RE.sub("", "".join(_chunks))


def _reaper() -> None:
    """Background thread: free any model idle longer than its TTL."""
    while True:
        time.sleep(REAPER_INTERVAL)
        now = time.time()
        with _lock:
            for spec in _SPECS.values():
                if spec.handle is not None and (now - spec.last_used) > spec.ttl:
                    spec.unload()


def _run_fim(prefix: str, suffix: str, max_tokens: int) -> str:
    prompt = f"<|fim_prefix|>{prefix}<|fim_suffix|>{suffix}<|fim_middle|>"
    return _generate("fim", prompt, max_tokens)


def _clean_commit_message(text: str) -> str:
    """Reduce raw model output to a single clean commit subject line.

    Handles common instruct-model quirks: markdown code fences, a leading
    'Commit message:' label, surrounding quotes/backticks, and a subject the
    model split across several lines (joined back into one line). Only a blank
    line ends the subject — a hard line break inside it is collapsed to a space.
    """
    t = text.strip()
    # Drop surrounding markdown code fences (```), if present.
    if t.startswith("```"):
        lines = t.splitlines()
        if lines and lines[0].lstrip().startswith("```"):
            lines = lines[1:]
        if lines and lines[-1].strip().startswith("```"):
            lines = lines[:-1]
        t = "\n".join(lines).strip()
    # First paragraph (up to the first blank line) = the subject; collapse any
    # internal line breaks into single spaces.
    para = []
    for line in t.splitlines():
        if not line.strip():
            if para:
                break
            continue
        para.append(line.strip())
    subject = " ".join(para)
    # Strip a leading label and any surrounding quotes/backticks.
    subject = re.sub(r"(?i)^\s*commit message\s*:\s*", "", subject)
    subject = subject.strip().strip("`\"'").strip()
    return subject or t


def _run_commit(diff: str, max_tokens: int) -> str:
    diff = diff[:MAX_DIFF_CHARS]
    prompt = (
        "<|im_start|>system\n" + COMMIT_SYSTEM_PROMPT + "<|im_end|>\n"
        "<|im_start|>user\n" + diff + "<|im_end|>\n"
        "<|im_start|>assistant\n"
    )
    text = _generate("commit", prompt, max_tokens)
    print(f"[commit] raw output: {text!r}", flush=True)   # TEMP DEBUG
    return _clean_commit_message(text)


# ---------------------------------------------------------------------------
# HTTP layer
# ---------------------------------------------------------------------------

class _Handler(BaseHTTPRequestHandler):
    def log_message(self, format, *args):  # noqa: A002 (name dictated by stdlib)
        sys.stderr.write(f"[http] {self.address_string()} {format % args}\n")

    def _reply(self, code: int, payload: dict) -> None:
        body = json.dumps(payload).encode("utf-8")
        try:
            self.send_response(code)
            self.send_header("Content-Type", "application/json")
            self.send_header("Content-Length", str(len(body)))
            self.end_headers()
            self.wfile.write(body)
        except (BrokenPipeError, ConnectionResetError):
            # Client cancelled (the inline-suggestion plugin kills requests
            # on every keystroke). Not an error worth logging.
            pass

    def _read_json(self):
        n = int(self.headers.get("Content-Length", "0"))
        return json.loads(self.rfile.read(n) or b"{}")

    def handle_one_request(self):  # noqa: D401
        try:
            super().handle_one_request()
        except (BrokenPipeError, ConnectionResetError):
            self.close_connection = True

    def do_POST(self):
        if self.path == "/infill":
            self._do_infill()
        elif self.path == "/commit":
            self._do_commit()
        elif self.path == "/stt":
            self._do_stt()
        else:
            self._reply(404, {"error": "unknown endpoint (try /infill or /commit)"})

    def _do_infill(self):
        try:
            body = self._read_json()
        except json.JSONDecodeError as exc:
            self._reply(400, {"error": f"bad json: {exc}"})
            return
        prefix = body.get("input_prefix", "")
        suffix = body.get("input_suffix", "")
        n_predict = int(body.get("n_predict") or FIM_MAX_NEW_TOKENS)
        try:
            content = _run_fim(prefix, suffix, n_predict)
        except Exception as exc:
            self._reply(500, {"error": str(exc)})
            return
        self._reply(200, {"content": content})

    def _do_commit(self):
        try:
            body = self._read_json()
        except json.JSONDecodeError as exc:
            self._reply(400, {"error": f"bad json: {exc}"})
            return
        diff = body.get("diff", "")
        if not diff.strip():
            self._reply(400, {"error": "empty diff"})
            return
        max_tokens = int(body.get("max_tokens") or COMMIT_MAX_NEW_TOKENS)
        try:
            message = _run_commit(diff, max_tokens)
        except Exception as exc:
            self._reply(500, {"error": str(exc)})
            return
        self._reply(200, {"message": message})

    def _do_stt(self):
        content_type = self.headers.get("Content-Type", "")
        if not content_type:
            self._reply(400, {"error": "missing Content-Type"})
            return

        parts = content_type.split(";")
        media_type = parts[0].strip()

        if media_type != "audio/l16":
            self._reply(415, {"error": "unsupported media type; expected audio/l16"})
            return

        params = {}
        for p in parts[1:]:
            p = p.strip()
            if "=" in p:
                k, v = p.split("=", 1)
                params[k.strip()] = v.strip()

        rate = int(params.get("rate", "0"))
        channels = int(params.get("channels", "0"))

        if rate != 16000:
            self._reply(400, {"error": f"unsupported sample rate {rate}; expected 16000"})
            return
        if channels != 1:
            self._reply(400, {"error": f"unsupported channels {channels}; expected 1"})
            return

        pcm_data = self._read_chunked_body()

        rec = sherpa_onnx.OnlineRecognizer()
        stream = rec.create_stream()
        if pcm_data:
            stream.accept_waveform(16000, pcm_data)
        text = rec.get_result(stream)
        self._reply(200, {"text": text})

    def _read_chunked_body(self) -> bytes:
        transfer_encoding = self.headers.get("Transfer-Encoding", "")
        if transfer_encoding.lower() != "chunked":
            n = int(self.headers.get("Content-Length", "0"))
            return self.rfile.read(n)

        data = bytearray()
        while True:
            line = b""
            while not line.endswith(b"\r\n"):
                ch = self.rfile.read(1)
                if not ch:
                    break
                line += ch
            if not line:
                break
            chunk_size = int(line.rstrip(b"\r\n").split(b";")[0], 16)
            if chunk_size == 0:
                self.rfile.read(2)
                break
            chunk_data = self.rfile.read(chunk_size)
            data.extend(chunk_data)
            self.rfile.read(2)
        return bytes(data)


def _init_runtime() -> None:
    if not LIB_PATH.exists():
        sys.exit(f"librkllmrt.so missing at {LIB_PATH}")
    # Models load lazily on first request; nothing is loaded at startup.
    print("[rkllm] runtime ready; models load on demand", flush=True)


def main() -> None:
    _init_runtime()
    threading.Thread(target=_reaper, daemon=True).start()
    httpd = ThreadingHTTPServer((HOST, PORT), _Handler)
    print(f"[http] listening on http://{HOST}:{PORT}", flush=True)
    try:
        httpd.serve_forever()
    except KeyboardInterrupt:
        pass
    finally:
        with _lock:
            for spec in _SPECS.values():
                spec.unload("shutdown")
        print("[rkllm] destroyed all", flush=True)


if __name__ == "__main__":
    main()
