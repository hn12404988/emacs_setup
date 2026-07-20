# local-llm — Qwen2.5-Coder on RK3588 NPU for Emacs

A minimal HTTP shim that runs Rockchip's **rkllm** runtime on the NanoPi M6's
NPU and serves two endpoints for Emacs:

- `POST /infill` — FIM code completion for `inline-suggestion.el` (ghost text).
- `POST /commit` — turns a `git diff` into a one-line commit message (Magit
  `C-x c`).
- `POST /stt`   — **speech-to-text** (Zipformer transducer on NPU, bilingual
  zh-en).

Two models are served. They are **loaded lazily on first use and freed after an
idle TTL**, so an idle daemon holds almost no memory (the FIM model is no longer
pinned in RAM all day). The NPU runs one inference at a time, so requests are
serialized by a single lock.

## Layout

```
.
├── models/
│   ├── Qwen2.5-Coder-1.5B-…rkllm       (~2 GB, FIM)
│   ├── Qwen2.5-Coder-3B-Instruct.rkllm  (~3.7 GB, commit)
│   └── sherpa-onnx-rk3588-…/            (~124 MB, ASR — encoder/decoder/joiner.rknn)
├── bin/sherpa-onnx                       RKNN-enabled CLI (compiled on-device)
├── lib/librkllmrt.so                     runtime v1.1.4 (gitignored)
├── include/rkllm.h                       C header, kept only for ctypes reference
├── server.py                             the shim: ModelManager + /infill + /commit + /stt
├── local-llm.service                     systemd user unit (installed via `make install`)
├── Makefile                              make up/down/status/logs/install
└── README.md
```

`models/` and `lib/*.so` are gitignored — fetch them as below.

## Models (lazy-loaded, evicted on idle)

| key | model | size | TTL | endpoint |
|---|---|---|---|---|
| `fim` | `Qwen2.5-Coder-1.5B` base, w8a8 | ~2 GB | 30 min | `/infill` |
| `commit` | `Qwen2.5-Coder-3B-Instruct`, w8a8 | ~3.7 GB | 10 min | `/commit` |
| `asr` | `Zipformer` transducer, bilingual zh-en, RKNN | ~124 MB | — (one-shot CLI) | `/stt` |

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

# STT: send 16-bit 16kHz mono PCM, get back transcription
curl -s -X POST http://localhost:8080/stt \
  -H "Content-Type: audio/l16; rate=16000; channels=1" \
  --data-binary @/path/to/raw_audio.pcm
# -> {"text": "昨天是 MONDAY TODAY IS LIBRAR THE DAY AFTER TOMORROW是星期三"}
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
# ASR model (Zipformer bilingual zh-en, RKNN, ~124 MB)
curl -L -o models/sherpa-onnx-rk3588-streaming-zipformer-bilingual-zh-en-2023-02-20.tar.bz2 \
  https://huggingface.co/csukuangfj/sherpa-onnx-rknn-models/resolve/main/streaming-asr/sherpa-onnx-rk3588-streaming-zipformer-bilingual-zh-en-2023-02-20.tar.bz2
tar xf models/sherpa-onnx-rk3588-streaming-zipformer-bilingual-zh-en-2023-02-20.tar.bz2 -C models/
rm models/sherpa-onnx-rk3588-streaming-zipformer-bilingual-zh-en-2023-02-20.tar.bz2
```

### RKNN runtime (for ASR on NPU)

The ASR model needs the **RKNN runtime 2.2.0** (`librknnrt.so`).  It is NOT
the same as `librkllmrt.so` (which is for LLM models only).

```sh
# Clone the rknn-toolkit2 repo at tag v2.2.0 and copy the aarch64 runtime
git clone --depth 1 --branch v2.2.0 https://github.com/airockchip/rknn-toolkit2.git /tmp/rknn-toolkit2
sudo cp /tmp/rknn-toolkit2/rknpu2/runtime/Linux/librknn_api/aarch64/librknnrt.so /usr/local/lib/
sudo cp /tmp/rknn-toolkit2/rknpu2/runtime/Linux/librknn_api/include/rknn_api.h /usr/local/include/
sudo ldconfig
rm -rf /tmp/rknn-toolkit2
```

### sherpa-onnx CLI (compiled on-device)

The `/stt` endpoint calls a standalone `sherpa-onnx` binary built with
`-DSHERPA_ONNX_ENABLE_RKNN=ON`.  It must be compiled on the RK3588 itself:

```sh
git clone --depth 1 https://github.com/k2-fsa/sherpa-onnx /tmp/sherpa-onnx
cd /tmp/sherpa-onnx && mkdir build && cd build
cmake \
  -DSHERPA_ONNX_ENABLE_RKNN=ON \
  -DSHERPA_ONNX_ENABLE_PORTAUDIO=OFF \
  -DSHERPA_ONNX_ENABLE_WEBSOCKET=OFF \
  -DSHERPA_ONNX_ENABLE_TTS=OFF \
  -DSHERPA_ONNX_ENABLE_SPEAKER_DIARIZATION=OFF \
  -DCMAKE_BUILD_TYPE=Release ..
make -j$(nproc) sherpa-onnx
cp bin/sherpa-onnx /path/to/local-llm/bin/
```

**Why not `pip install`?**  The standard sherpa-onnx wheel only ships ONNX
Runtime (CPU).  The RKNN backend needs a from-source build.  The Python
bindings also fail to build on aarch64 due to pybind11/FindPython issues,
so the CLI is called via subprocess.  The 0.9 s model-load overhead per
request is acceptable for the push-to-talk use case.

## Client setup (macOS / Linux laptop)

The **stt-daemon** captures your microphone and streams audio to this server.
Install it on your laptop with one command:

```sh
# Grab the script
curl -O https://raw.githubusercontent.com/hn12404988/emacs_setup/main/install-stt-client.sh
chmod +x install-stt-client.sh

# Install (replace IP with this server's Tailscale address)
./install-stt-client.sh --server http://<RK3588_TAILSCALE_IP>:8080/stt
```

The script detects your OS and architecture, downloads the right binary,
and sets up auto-start (LaunchAgent on macOS, systemd on Linux).

**Ghostty keybind** — add to your Ghostty config:
```
keybind = super+shift+t=sh -c "curl -s http://localhost:9876/toggle"
```

**How it works:**
```
macOS/Linux laptop                    NanoPi M6 (RK3588)
──────────────────                    ──────────────────
Ghostty keybind
  → curl localhost:9876/toggle
       │
  stt-daemon (Rust)
    ├─ cpal: record 16kHz mono PCM
    ├─ chunked POST ────────────────→ POST /stt (server.py)
    │                                  └→ bin/sherpa-onnx (NPU)
    └─ paste text at cursor ←────────── {"text": "..."}
```

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
- **ASR model:** `Zipformer` transducer, bilingual zh-en, compiled with RKNN
  toolkit 2.1.0.  Must be paired with **RKNN runtime ≥ 2.1.0** (2.2.0 is the
  known-good version; 1.5.2 crashes with `GatherElements` not supported).
  The model runs at 0.23 RTF on the NPU (~2.3 s for 10 s of audio).
- **ASR executable:** `bin/sherpa-onnx` is a clone-and-compile artifact — it
  is checked into git at 20 MB so the server can be deployed without
  recompiling.

## Troubleshooting

- **`librkllmrt.so missing` / `model missing`**: re-run the download commands.
- **`rkllm_init failed (rc=...)`**: model/runtime version mismatch, or NPU
  driver too old (need ≥ 0.9.6). Check `cat /sys/kernel/debug/rknpu/version`.
- **High latency**: NPU throttling — `cat /sys/class/devfreq/fdab0000.npu/cur_freq`.
  Also remember the first call after an idle TTL pays the cold-start load.
- **Commit message looks like chat/rambles**: tune `COMMIT_SYSTEM_PROMPT`, or
  lower `COMMIT_MAX_NEW_TOKENS` / `MAX_DIFF_CHARS` in `server.py`.
- **STT returns "no JSON in CLI output"**: the `sherpa-onnx` binary is
  missing or was built without `-DSHERPA_ONNX_ENABLE_RKNN=ON`.  Rebuild.
- **STT crash with "unsupport GatherElements op"**: RKNN runtime is too old.
  Install `librknnrt.so` ≥ 2.1.0 from `airockchip/rknn-toolkit2` tag `v2.2.0`.
- **Client can't reach server**: check Tailscale connectivity (`ping <ip>`)
  and that the `--server` URL in `install-stt-client.sh` uses port **8080**.
