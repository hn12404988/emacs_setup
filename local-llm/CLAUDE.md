# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

A small HTTP shim (`server.py`) that runs on a NanoPi M6 (Rockchip RK3588 NPU). It loads `lib/librkllmrt.so` via `ctypes` and exposes a `llama.cpp`-compatible `POST /infill` endpoint on `127.0.0.1:8080`, so an existing Emacs `inline-suggestion.el` client can request fill-in-the-middle (FIM) completions from the local Qwen2.5-Coder-1.5B model without any client changes.

The whole project is the shim plus its pinned runtime artifacts. There is no build step, no package manager, no test suite, and no virtualenv — `server.py` uses only the Python stdlib.

## Common commands

```sh
./start.sh           # nohup python3 server.py; pid -> /tmp/local-llm.pid, log -> /tmp/local-llm.log
./stop.sh            # SIGTERM the pid in /tmp/local-llm.pid
tail -f /tmp/local-llm.log
python3 server.py    # run in foreground (useful when iterating on server.py)
```

Smoke test (`curl` against a running server):

```sh
curl -s -X POST http://localhost:8080/infill \
  -H 'Content-Type: application/json' \
  -d '{"input_prefix":"def fibonacci(n):\n    if n < 2:\n        return n\n    return ","input_suffix":"\n\nprint(fibonacci(10))","n_predict":40,"temperature":0,"stream":false}'
```

Expect `{"content": "..."}` in roughly 4–7 s on the NPU. There is no other test harness — verify changes by running this curl and reading the response.

## Architecture notes that are not obvious from a single file

### The whole runtime is version-locked to rkllm 1.1.2

`server.py`'s `ctypes.Structure` definitions (`RKLLMParam`, `RKLLMExtendParam`, `RKLLMInferParam`, `RKLLMResult`, etc.) mirror the exact field layout of `include/rkllm.h` from rkllm v1.1.2. The `.rkllm` model file in `models/` was also produced by toolkit 1.1.2 and will be rejected by newer runtimes. Treat all three — `lib/librkllmrt.so`, `models/*.rkllm`, and the structs in `server.py` — as one unit. If any of them changes, all three usually have to change together. See README.md "To upgrade rkllm" for the full re-conversion procedure.

When editing the structs, always cross-check against `include/rkllm.h`. The `RKLLMExtendParam.reserved` byte count and the union ordering in `_RKLLMInputUnion` are easy to get wrong and segfault silently.

### Inference is single-context and serialized through module globals

There is exactly one rkllm handle (`_handle`) for the lifetime of the process. Concurrent requests are serialized by `_inf_lock` in `_run_fim`. The C callback (`_on_result`) is a `@LLMResultCallback`-decorated function that writes into module-level globals (`_chunks`, `_aborted`, `_token_count`, `_max_tokens`). This works only because the lock guarantees one in-flight request at a time — do not refactor the callback to instance state without also restructuring how the C runtime is called.

### Generation is stopped by aborting from inside the callback

rkllm has no "stop after N tokens" parameter at the per-request level. Instead, `_on_result` calls `rkllm.rkllm_abort(_handle)` once the per-request cap (`_max_tokens`) or a Qwen FIM stop-token id is hit. After abort, `rkllm_run` returns a non-zero rc; `_run_fim` swallows that rc when `_aborted` is true (it is the expected path), and only raises when rc != 0 *and* we did not abort.

`STOP_TOKEN_IDS` is the set of Qwen2.5-Coder special-token ids that should end FIM generation: `<|endoftext|>` 151643, `<|im_end|>` 151645, `<|fim_pad|>` 151662, `<|file_sep|>` 151664. The numbers come from `Qwen/Qwen2.5-Coder-1.5B/tokenizer_config.json`. If you change the model, update this set.

### The FIM prompt format is hard-coded to Qwen

`_run_fim` builds the prompt as `<|fim_prefix|>{prefix}<|fim_suffix|>{suffix}<|fim_middle|>`. Other base FIM models (DeepSeek-Coder, StarCoder2, CodeLlama) use different sentinel tokens — the literal string here would have to change.

The model **must** be the *base* Qwen2.5-Coder, not the *Instruct* variant. Instruct has no FIM template and will produce chat-style replies.

### Special-token stripping is a belt-and-braces fallback

`param.skip_special_token = True` already tells rkllm 1.1.2 to drop FIM/chat sentinels from the decoded text, but the runtime occasionally leaks them anyway (notably the FIM ones). `_SPECIAL_TOKEN_RE` is applied to the joined output as a safety net before returning. Keep both layers — removing either one has produced visible artifacts in past sessions.

### Client-cancel is the normal case, not an error

The Emacs `inline-suggestion.el` plugin cancels in-flight requests on every keystroke, which surfaces as `BrokenPipeError` / `ConnectionResetError` on `self.wfile.write`. `_Handler._reply` and `handle_one_request` swallow those silently; do not add logging for them or the log will fill up during normal editing.

## File map

| Path | Purpose |
| --- | --- |
| `server.py` | The whole shim — ctypes bindings, single-context inference, HTTP handler. |
| `lib/librkllmrt.so` | Rockchip rkllm runtime, **v1.1.2 only**. |
| `include/rkllm.h` | Reference for the ctypes struct layouts in `server.py`. Not compiled. |
| `models/*.rkllm` | The pre-converted Qwen2.5-Coder-1.5B base model (w8a8). |
| `start.sh` / `stop.sh` | Foreground/background lifecycle via `/tmp/local-llm.pid`. |
| `README.md` | User-facing setup, Emacs config, troubleshooting (NPU driver, freq, etc.). |
