#!/usr/bin/env python3
"""rkllm /infill shim — translates llama.cpp FIM API to Rockchip rkllm runtime.

POST /infill
  request:  {"input_prefix": str, "input_suffix": str,
             "n_predict": int, "temperature": float, "stream": false}
  response: {"content": str}

Pinned to rkllm runtime v1.1.2 — struct layouts and the ABI here track that
release. Upgrading librkllmrt.so requires re-checking rkllm.h.
"""

import ctypes
import json
import re
import sys
import threading
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path

# Qwen special-token markers that occasionally leak through skip_special_token
# in rkllm v1.1.2 (notably the FIM ones). Stripped post-decode as a safety net.
_SPECIAL_TOKEN_RE = re.compile(
    r"<\|(?:fim_(?:prefix|suffix|middle|pad)"
    r"|repo_name|file_sep|endoftext|im_(?:start|end))\|>"
)

HERE = Path(__file__).resolve().parent
LIB_PATH = HERE / "lib" / "librkllmrt.so"
MODEL_PATH = HERE / "models" / "Qwen2.5-Coder-1.5B-rk3588-w8a8-opt-1-hybrid-ratio-0.0.rkllm"
HOST, PORT = "127.0.0.1", 8080

MAX_CONTEXT_LEN = 2048
MAX_NEW_TOKENS = 100  # hard cap per request; runtime sets this at init time

# Qwen2.5-Coder special token ids that should halt generation in FIM mode.
# Source: Qwen/Qwen2.5-Coder-1.5B/tokenizer_config.json
#   151643 <|endoftext|>, 151645 <|im_end|>,
#   151662 <|fim_pad|>,   151664 <|file_sep|>
STOP_TOKEN_IDS = {151643, 151645, 151662, 151664}


# ---------------------------------------------------------------------------
# ctypes mappings for rkllm v1.1.2
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
# Inference state — single context, serialized by _inf_lock
# ---------------------------------------------------------------------------

_inf_lock = threading.Lock()
_handle = LLMHandle()
_chunks: list[str] = []
_aborted = False
_token_count = 0
_max_tokens = MAX_NEW_TOKENS  # per-request cap, set in _run_fim


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
    hit_stop = res.token_id in STOP_TOKEN_IDS
    if (hit_cap or hit_stop) and not _aborted:
        _aborted = True
        rkllm.rkllm_abort(_handle)


def _init_runtime() -> None:
    if not LIB_PATH.exists():
        sys.exit(f"librkllmrt.so missing at {LIB_PATH}")
    if not MODEL_PATH.exists():
        sys.exit(f"model missing at {MODEL_PATH}")
    param = rkllm.rkllm_createDefaultParam()
    param.model_path = str(MODEL_PATH).encode()
    param.max_context_len = MAX_CONTEXT_LEN
    param.max_new_tokens = MAX_NEW_TOKENS
    param.top_k = 1
    param.top_p = 0.9
    param.temperature = 0.0
    param.repeat_penalty = 1.1
    param.skip_special_token = True
    param.is_async = False
    rc = rkllm.rkllm_init(ctypes.byref(_handle), ctypes.byref(param), _on_result)
    if rc != 0:
        sys.exit(f"rkllm_init failed (rc={rc})")
    print(f"[rkllm] loaded {MODEL_PATH.name}", flush=True)


def _run_fim(prefix: str, suffix: str, max_tokens: int) -> str:
    global _chunks, _aborted, _token_count, _max_tokens
    prompt = f"<|fim_prefix|>{prefix}<|fim_suffix|>{suffix}<|fim_middle|>"
    with _inf_lock:
        _chunks = []
        _aborted = False
        _token_count = 0
        _max_tokens = max(1, min(max_tokens, MAX_NEW_TOKENS))
        rkin = RKLLMInput()
        rkin.input_type = RKLLM_INPUT_PROMPT
        rkin.prompt_input = prompt.encode("utf-8")
        infer = RKLLMInferParam()
        infer.mode = RKLLM_INFER_GENERATE
        infer.lora_params = None
        infer.prompt_cache_params = None
        rc = rkllm.rkllm_run(_handle, ctypes.byref(rkin), ctypes.byref(infer), None)
        if rc != 0 and not _aborted:
            raise RuntimeError(f"rkllm_run failed (rc={rc})")
        return _SPECIAL_TOKEN_RE.sub("", "".join(_chunks))


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

    def handle_one_request(self):  # noqa: D401
        try:
            super().handle_one_request()
        except (BrokenPipeError, ConnectionResetError):
            self.close_connection = True

    def do_POST(self):
        if self.path != "/infill":
            self._reply(404, {"error": "only /infill is supported"})
            return
        n = int(self.headers.get("Content-Length", "0"))
        try:
            body = json.loads(self.rfile.read(n) or b"{}")
        except json.JSONDecodeError as exc:
            self._reply(400, {"error": f"bad json: {exc}"})
            return
        prefix = body.get("input_prefix", "")
        suffix = body.get("input_suffix", "")
        n_predict = int(body.get("n_predict") or MAX_NEW_TOKENS)
        try:
            content = _run_fim(prefix, suffix, n_predict)
        except Exception as exc:
            self._reply(500, {"error": str(exc)})
            return
        self._reply(200, {"content": content})


def main() -> None:
    _init_runtime()
    httpd = ThreadingHTTPServer((HOST, PORT), _Handler)
    print(f"[http] listening on http://{HOST}:{PORT}", flush=True)
    try:
        httpd.serve_forever()
    except KeyboardInterrupt:
        pass
    finally:
        rkllm.rkllm_destroy(_handle)
        print("[rkllm] destroyed", flush=True)


if __name__ == "__main__":
    main()
