//! stt-daemon — microphone capture, streaming, and speech-to-text paste daemon.
//!
//! Listens on a local HTTP port for toggle requests, captures mic audio via cpal,
//! streams PCM to a remote STT server, and types transcribed text at the cursor.

use axum::{response::Json, routing::get, Router};
use serde::Serialize;

#[derive(Serialize)]
struct ToggleResponse {
    recording: bool,
}

async fn toggle() -> Json<ToggleResponse> {
    Json(ToggleResponse { recording: false })
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    let port: u16 = std::env::var("STT_PORT")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(9876);

    let app = Router::new().route("/toggle", get(toggle));

    let addr = format!("127.0.0.1:{port}");
    let listener = tokio::net::TcpListener::bind(&addr).await?;
    eprintln!("stt-daemon listening on {addr}");

    axum::serve(listener, app).await?;
    Ok(())
}
