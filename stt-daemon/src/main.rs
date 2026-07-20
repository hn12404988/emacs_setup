//! stt-daemon — microphone capture, streaming, and speech-to-text paste daemon.
//!
//! Listens on a local HTTP port for toggle requests, captures mic audio via cpal,
//! streams PCM to a remote STT server, and types transcribed text at the cursor.

use axum::{extract::Extension, response::Json, routing::get, Router};
use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};
use serde::Serialize;
use std::process::Command;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::Duration;

#[derive(Serialize)]
struct ToggleResponse {
    recording: bool,
}

struct AppState {
    recording: AtomicBool,
    pcm_buffer: Arc<Mutex<Vec<u8>>>,
    stt_server: String,
    stop_signal: Arc<AtomicBool>,
    capture_thread: Mutex<Option<thread::JoinHandle<()>>>,
}

fn type_text(text: &str) {
    #[cfg(target_os = "macos")]
    {
        let script = format!(
            "tell application \"System Events\" to keystroke \"{}\"",
            text.replace('\"', "\\\"")
        );
        if let Err(e) = Command::new("osascript").arg("-e").arg(&script).output() {
            eprintln!("osascript failed: {e}");
        }
    }
    #[cfg(target_os = "linux")]
    {
        if let Err(e) = Command::new("wtype").arg(text).output() {
            eprintln!("wtype failed: {e}");
        }
    }
}

async fn toggle(Extension(state): Extension<Arc<AppState>>) -> Json<ToggleResponse> {
    let was_recording = state.recording.fetch_xor(true, Ordering::SeqCst);
    let now_recording = !was_recording;

    if now_recording {
        state.pcm_buffer.lock().unwrap().clear();
        state.stop_signal.store(false, Ordering::SeqCst);

        let buffer = Arc::clone(&state.pcm_buffer);
        let stop = Arc::clone(&state.stop_signal);

        let handle = thread::spawn(move || {
            let host = cpal::default_host();
            let device = match host.default_input_device() {
                Some(d) => d,
                None => {
                    eprintln!("no input device available");
                    return;
                }
            };
            let supported = match device.default_input_config() {
                Ok(c) => c,
                Err(e) => {
                    eprintln!("input config error: {e}");
                    return;
                }
            };
            let config = supported.config();

            let stream = match build_stream(&device, &supported, &config, &buffer) {
                Ok(s) => s,
                Err(e) => {
                    eprintln!("stream build error: {e}");
                    return;
                }
            };

            if stream.play().is_err() {
                eprintln!("stream play error");
                return;
            }

            while !stop.load(Ordering::SeqCst) {
                thread::sleep(Duration::from_millis(10));
            }

            drop(stream);
        });

        *state.capture_thread.lock().unwrap() = Some(handle);
    } else {
        state.stop_signal.store(true, Ordering::SeqCst);

        if let Some(handle) = state.capture_thread.lock().unwrap().take() {
            let _ = handle.join();
        }

        let pcm_data = {
            let mut buf = state.pcm_buffer.lock().unwrap();
            std::mem::take(&mut *buf)
        };

        if !pcm_data.is_empty() {
            let client = reqwest::Client::new();
            let stt_url = state.stt_server.clone();
            tokio::spawn(async move {
                match client.post(&stt_url).body(pcm_data).send().await {
                    Ok(resp) => {
                        match resp.json::<serde_json::Value>().await {
                            Ok(json) => {
                                if let Some(text) = json.get("text").and_then(|v| v.as_str()) {
                                    if !text.is_empty() {
                                        type_text(text);
                                    }
                                }
                            }
                            Err(e) => eprintln!("STT response parse failed: {e}"),
                        }
                    }
                    Err(e) => eprintln!("STT POST failed: {e}"),
                }
            });
        }
    }

    Json(ToggleResponse {
        recording: now_recording,
    })
}

fn build_stream(
    device: &cpal::Device,
    supported: &cpal::SupportedStreamConfig,
    config: &cpal::StreamConfig,
    buffer: &Arc<Mutex<Vec<u8>>>,
) -> anyhow::Result<cpal::Stream> {
    let buf = Arc::clone(buffer);
    let err_fn = |err| eprintln!("cpal error: {err}");

    let stream = match supported.sample_format() {
        cpal::SampleFormat::I16 => device.build_input_stream(
            config,
            move |data: &[i16], _: &cpal::InputCallbackInfo| {
                let mut b = buf.lock().unwrap();
                for sample in data {
                    b.extend_from_slice(&sample.to_le_bytes());
                }
            },
            err_fn,
            None,
        )?,
        cpal::SampleFormat::U16 => device.build_input_stream(
            config,
            move |data: &[u16], _: &cpal::InputCallbackInfo| {
                let mut b = buf.lock().unwrap();
                for sample in data {
                    b.extend_from_slice(&sample.to_le_bytes());
                }
            },
            err_fn,
            None,
        )?,
        cpal::SampleFormat::F32 => device.build_input_stream(
            config,
            move |data: &[f32], _: &cpal::InputCallbackInfo| {
                let mut b = buf.lock().unwrap();
                for sample in data {
                    b.extend_from_slice(&sample.to_le_bytes());
                }
            },
            err_fn,
            None,
        )?,
        _ => anyhow::bail!("unsupported sample format"),
    };

    Ok(stream)
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let port: u16 = std::env::var("STT_PORT")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(9876);

    let stt_server = std::env::var("STT_SERVER")
        .unwrap_or_else(|_| "http://127.0.0.1:9090/stt".to_string());

    let state = Arc::new(AppState {
        recording: AtomicBool::new(false),
        pcm_buffer: Arc::new(Mutex::new(Vec::new())),
        stt_server,
        stop_signal: Arc::new(AtomicBool::new(false)),
        capture_thread: Mutex::new(None),
    });

    let app = Router::new()
        .route("/toggle", get(toggle))
        .layer(Extension(state));

    let addr = format!("127.0.0.1:{port}");
    let listener = tokio::net::TcpListener::bind(&addr).await?;
    eprintln!("stt-daemon listening on {addr}");

    axum::serve(listener, app).await?;
    Ok(())
}
