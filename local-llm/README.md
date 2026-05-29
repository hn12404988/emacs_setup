# local-llm — Qwen2.5-Coder-1.5B on RK3588 NPU for Emacs FIM

A minimal HTTP shim that runs Rockchip's **rkllm** runtime on the NanoPi M6's
NPU and exposes a llama.cpp-compatible `/infill` endpoint, so the existing
`inline-suggestion.el` plugin works without changes.

## Layout

```
.
├── models/Qwen2.5-Coder-1.5B-rk3588-w8a8-opt-1-hybrid-ratio-0.0.rkllm  (2 GB)
├── lib/librkllmrt.so       runtime v1.1.2 (must match the model)
├── include/rkllm.h         C header, kept only for ctypes reference
├── server.py               the /infill shim (stdlib only)
├── start.sh / stop.sh
└── README.md
```

## Run

```sh
./start.sh        # boots the server, writes /tmp/local-llm.pid + .log
./stop.sh         # SIGTERM the server
tail -f /tmp/local-llm.log
```

Smoke test:

```sh
curl -s -X POST http://localhost:8080/infill \
  -H 'Content-Type: application/json' \
  -d '{"input_prefix":"def fibonacci(n):\n    if n < 2:\n        return n\n    return ","input_suffix":"\n\nprint(fibonacci(10))","n_predict":40,"temperature":0,"stream":false}' \
  | python3 -m json.tool
```

Expect `{"content": "fibonacci(n - 1) + fibonacci(n - 2)"}` (or similar) in
roughly 4–7 s.

## Emacs config

The `inline-suggestion.el` plugin tries to auto-start `llama-server`. We start
our own server, so disable autostart:

```elisp
(with-eval-after-load 'inline-suggestion
  (setq inline-suggestion-server-autostart nil)
  (setq inline-suggestion-server-url "http://localhost:8080")
  (setq inline-suggestion-max-tokens 60))   ; lower = snappier ghost text
```

Then `M-x inline-suggestion-mode` in any buffer.

## Why these specific versions

- **Model**: `Qwen2.5-Coder-1.5B` *base* (NOT Instruct). Base is FIM-trained.
  Pre-converted to `.rkllm` w8a8 by HF user `locainf`.
- **Runtime: v1.1.2**. The `.rkllm` file is version-locked to the toolkit
  version it was built with. Newer runtimes will reject the file.
- **Quant: w8a8**. Plain int8 weights + int8 activations. ~2 GB on disk,
  ~2.5 GB resident, decode ~12–18 tok/s on RK3588.

## To upgrade rkllm

You can't, without re-converting the model. To use rkllm 1.2.x you would need
an x86 Linux box with `rkllm-toolkit` 1.2.x and the original
`Qwen/Qwen2.5-Coder-1.5B` safetensors. Then drop in the new `.rkllm` and
matching `librkllmrt.so`.

## Troubleshooting

- **`librkllmrt.so missing`**: the wget failed. Repull from the v1.1.2 release
  tag at `airockchip/rknn-llm`.
- **`rkllm_init failed (rc=...)`**: usually a model/runtime version mismatch,
  or the NPU driver is too old (need at least v0.9.6). Check
  `cat /sys/kernel/debug/rknpu/version`.
- **High latency**: NPU may be throttling. Verify it is not stuck at low
  freq: `cat /sys/class/devfreq/fdab0000.npu/cur_freq`.
- **Model emits chat replies instead of code**: you grabbed the Instruct
  variant by mistake. The base FIM model has no chat template.
