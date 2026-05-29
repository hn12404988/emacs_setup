# local-llm — Qwen2.5-Coder on RK3588 NPU for Emacs

A minimal HTTP shim that runs Rockchip's **rkllm** runtime on the NanoPi M6's
NPU and serves two endpoints for Emacs:

- `POST /infill` — FIM code completion for `inline-suggestion.el` (ghost text).
- `POST /commit` — turns a `git diff` into a one-line commit message (Magit
  `C-x c`).

Two models are served. They are **loaded lazily on first use and freed after an
idle TTL**, so an idle daemon holds almost no memory (the FIM model is no longer
pinned in RAM all day). The NPU runs one inference at a time, so requests are
serialized by a single lock.

## Layout

```
.
├── models/Qwen2.5-Coder-1.5B-rk3588-w8a8-opt-1-hybrid-ratio-0.0.rkllm  (~2 GB, FIM)
├── models/Qwen2.5-Coder-3B-Instruct.rkllm                             (~3.7 GB, commit)
├── lib/librkllmrt.so       runtime v1.1.4 (gitignored)
├── include/rkllm.h         C header, kept only for ctypes reference
├── server.py               the shim (stdlib only): ModelManager + /infill + /commit
├── local-llm.service       systemd user unit (installed via `make install`)
├── Makefile                make up/down/status/logs/install
└── README.md
```

`models/` and `lib/*.so` are gitignored — fetch them as below.

## Models (lazy-loaded, evicted on idle)

| key | model | size | TTL | endpoint |
|---|---|---|---|---|
| `fim` | `Qwen2.5-Coder-1.5B` base, w8a8 | ~2 GB | 30 min | `/infill` |
| `commit` | `Qwen2.5-Coder-3B-Instruct`, w8a8 | ~3.7 GB | 10 min | `/commit` |

Cold start (first call after the model was evicted): ~3 s for FIM, ~5–8 s for
the 3B commit model. Warm calls are quick. TTLs are constants near the top of
`server.py`.

## Run

```sh
make up        # start the systemd user service (or: ./start.sh for foreground)
make logs      # journalctl -f — watch [rkllm] load/evict lines
make status
```

Smoke tests:

```sh
curl -s -X POST http://localhost:8080/infill -H 'Content-Type: application/json' \
  -d '{"input_prefix":"def add(a, b):\n    return ","input_suffix":"","n_predict":8}'
# -> {"content": "a + b ..."}

curl -s -X POST http://localhost:8080/commit -H 'Content-Type: application/json' \
  -d '{"diff":"diff --git a/x.py b/x.py\n@@\n-print(1)\n+print(2)\n"}'
# -> {"message": "fix: update print value"}
```

## Downloading the artifacts

No `git-lfs` / `huggingface-cli` needed — plain `curl` against HF resolve URLs:

```sh
# runtime v1.1.4 (.so), from the airockchip release tag
curl -L -o lib/librkllmrt.so \
  https://raw.githubusercontent.com/airockchip/rknn-llm/release-v1.1.4/rkllm-runtime/Linux/librkllm_api/aarch64/librkllmrt.so

# FIM model (base, FIM-trained)
curl -L -o models/Qwen2.5-Coder-1.5B-rk3588-w8a8-opt-1-hybrid-ratio-0.0.rkllm \
  https://huggingface.co/locainf/Qwen2.5-Coder-1.5B-rk3588-1.1.2/resolve/main/Qwen2.5-Coder-1.5B-rk3588-w8a8-opt-1-hybrid-ratio-0.0.rkllm

# commit model (instruction-tuned Coder)
curl -L -o models/Qwen2.5-Coder-3B-Instruct.rkllm \
  https://huggingface.co/3ib0n/Qwen2.5-3B-Coder-Instruct-rkllm/resolve/main/Qwen2.5-Coder-3B-Instruct.rkllm
```

## Why these specific versions

- **Runtime: v1.1.4.** Bumped from 1.1.2 so the 1.1.4-built commit model loads.
  The v1.1.2 and v1.1.4 `rkllm.h` headers are byte-identical, so `server.py`'s
  ctypes layout is unchanged. The old 1.1.2 FIM model **also loads** on the
  1.1.4 runtime (verified), so it did not need re-converting.
- **NPU driver:** 0.9.8 is enough for the 1.1.x runtime series. (1.2.x would
  need a newer driver.)
- **FIM model:** `Qwen2.5-Coder-1.5B` *base*, w8a8. Base is the cleanest for raw
  FIM. (Qwen2.5-Coder *Instruct* also supports the FIM tokens, so a 1.1.4
  Instruct 1.5B would work as a fallback if this base model ever needs replacing.)
- **Commit model:** `Qwen2.5-Coder-3B-Instruct`, w8a8 (~3.7 GB, ~7–9 tok/s).
  Instruction-tuned and code-aware — good at summarizing a diff while staying
  inside a ~10 s budget. A 7B build exists if you relax that budget.
- **Commit system prompt** is hardcoded in `server.py` as `COMMIT_SYSTEM_PROMPT`
  — edit it there to tune message style.

## Troubleshooting

- **`librkllmrt.so missing` / `model missing`**: re-run the download commands.
- **`rkllm_init failed (rc=...)`**: model/runtime version mismatch, or NPU
  driver too old (need ≥ 0.9.6). Check `cat /sys/kernel/debug/rknpu/version`.
- **High latency**: NPU throttling — `cat /sys/class/devfreq/fdab0000.npu/cur_freq`.
  Also remember the first call after an idle TTL pays the cold-start load.
- **Commit message looks like chat/rambles**: tune `COMMIT_SYSTEM_PROMPT`, or
  lower `COMMIT_MAX_NEW_TOKENS` / `MAX_DIFF_CHARS` in `server.py`.
