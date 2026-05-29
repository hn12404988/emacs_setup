# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

A small HTTP shim (`server.py`) that runs on a NanoPi M6 (Rockchip RK3588 NPU). It loads `lib/librkllmrt.so` via `ctypes` and exposes two endpoints on `127.0.0.1:8080`:

- `POST /infill` — `llama.cpp`-compatible FIM completion for the Emacs `inline-suggestion.el` client (small base Qwen2.5-Coder-1.5B model).
- `POST /commit` — `{"diff": ...}` → `{"message": ...}`, a one-line commit message from a larger instruction-tuned Qwen2.5-Coder-3B-Instruct model (Magit `C-x c`).

Models are **loaded lazily and freed after an idle TTL** by a small `ModelManager` (see below), so an idle daemon holds ~no model memory.

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

### The runtime is version-locked to rkllm 1.1.4

`server.py`'s `ctypes.Structure` definitions (`RKLLMParam`, `RKLLMExtendParam`, `RKLLMInferParam`, `RKLLMResult`, etc.) mirror the exact field layout of `include/rkllm.h`. The runtime is **v1.1.4**; the v1.1.2 and v1.1.4 headers are byte-identical, so the layout is unchanged from the 1.1.2 era. The commit `.rkllm` was built with toolkit 1.1.4; the older 1.1.2 FIM `.rkllm` also loads on the 1.1.4 runtime (verified). Treat `lib/librkllmrt.so`, `models/*.rkllm`, and the structs in `server.py` as one unit — if the `.so` version changes, re-check `rkllm.h` before trusting the structs.

When editing the structs, always cross-check against `include/rkllm.h`. The `RKLLMExtendParam.reserved` byte count and the union ordering in `_RKLLMInputUnion` are easy to get wrong and segfault silently.

### One inference at a time; models lazy-loaded and idle-evicted

Inference state lives in module globals (`_chunks`, `_aborted`, `_token_count`, `_max_tokens`, `_active_handle`, `_active_stop_ids`). A single `_lock` serializes **load, run, and destroy** across all models — the NPU does one thing at a time, so this also keeps the shared callback globals safe. Do not move that state to per-model instances without restructuring how the C runtime is driven.

`ModelManager` / `ModelSpec` own each model. `_generate(name, prompt, max_tokens)` loads the model on first use (`spec.load()` → `rkllm_init`), runs it, and stamps `last_used`. A daemon `_reaper` thread frees any model idle longer than its TTL (`spec.unload()` → `rkllm_destroy`). Because two handles can be live at once, the callback aborts via `_active_handle` (set per run under the lock), not a single global handle.

### Generation is stopped by aborting from inside the callback

rkllm has no "stop after N tokens" parameter at the per-request level. Instead, `_on_result` calls `rkllm.rkllm_abort(_active_handle)` once the per-request cap (`_max_tokens`) or a stop-token id (`_active_stop_ids`, set per run) is hit. After abort, `rkllm_run` returns a non-zero rc; `_generate` swallows that rc when `_aborted` is true (the expected path), and only raises when rc != 0 *and* we did not abort.

Stop-token sets (from `Qwen/Qwen2.5-Coder` `tokenizer_config.json`): FIM uses `FIM_STOP_IDS` = `<|endoftext|>` 151643, `<|im_end|>` 151645, `<|fim_pad|>` 151662, `<|file_sep|>` 151664; chat/commit uses `CHAT_STOP_IDS` = 151643, 151645. If you change a model, update its set.

### Prompt formats: FIM vs chat

`_run_fim` builds `<|fim_prefix|>{prefix}<|fim_suffix|>{suffix}<|fim_middle|>` (Qwen FIM sentinels; other model families differ). `_run_commit` builds the Qwen chat template (`<|im_start|>system ... <|im_end|>`) with the hardcoded `COMMIT_SYSTEM_PROMPT`, caps the diff to `MAX_DIFF_CHARS`, and returns the first non-empty line.

For pure FIM the *base* Qwen2.5-Coder is cleanest, but the *Instruct* variant also supports the FIM tokens — so a 1.1.4 Coder-Instruct 1.5B is a valid fallback for the FIM slot. The commit model is deliberately Instruct (it needs to follow instructions, not infill).

### Special-token stripping is a belt-and-braces fallback

`param.skip_special_token = True` already tells rkllm to drop FIM/chat sentinels from the decoded text, but the runtime occasionally leaks them anyway (notably the FIM ones). `_SPECIAL_TOKEN_RE` is applied to the joined output as a safety net before returning. Keep both layers — removing either one has produced visible artifacts in past sessions.

### Client-cancel is the normal case, not an error

The Emacs `inline-suggestion.el` plugin cancels in-flight requests on every keystroke, which surfaces as `BrokenPipeError` / `ConnectionResetError` on `self.wfile.write`. `_Handler._reply` and `handle_one_request` swallow those silently; do not add logging for them or the log will fill up during normal editing.

## File map

| Path | Purpose |
| --- | --- |
| `server.py` | The whole shim — ctypes bindings, `ModelManager` (lazy load + TTL), `/infill` + `/commit` handlers. |
| `lib/librkllmrt.so` | Rockchip rkllm runtime, **v1.1.4** (gitignored). |
| `include/rkllm.h` | Reference for the ctypes struct layouts in `server.py`. Not compiled. |
| `models/*-1.5B-*.rkllm` | FIM model: Qwen2.5-Coder-1.5B base (w8a8). Gitignored. |
| `models/*-3B-Instruct.rkllm` | Commit model: Qwen2.5-Coder-3B-Instruct (w8a8). Gitignored. |
| `local-llm.service` / `Makefile` | systemd user unit + `make up/down/status/logs/install`. |
| `start.sh` / `stop.sh` | Foreground/background lifecycle via `/tmp/local-llm.pid`. |
| `README.md` | User-facing setup, downloads, Emacs config, troubleshooting. |
