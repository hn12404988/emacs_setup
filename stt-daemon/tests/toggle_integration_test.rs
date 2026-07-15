//! Integration tests for stt-daemon.
//!
//! These tests spawn the stt-daemon binary, exercise its HTTP toggle endpoint,
//! and verify the full behavior chain: mic capture, PCM buffering, streaming to
//! STT server, and text typing.
//!
//! All tests are RED — the stub binary does not implement the toggle state
//! machine, mic capture, PCM buffering, or STT server streaming.

use std::io::{Read, Write};
use std::net::{TcpListener, TcpStream};
use std::process::{Child, Command};
use std::thread;
use std::time::Duration;

// ── helpers ────────────────────────────────────────────────────────────────

/// Find a free TCP port on localhost.
fn free_port() -> u16 {
    let listener = TcpListener::bind("127.0.0.1:0").expect("failed to bind to free port");
    listener.local_addr().unwrap().port()
}

/// Wait until `127.0.0.1:port` accepts connections (up to `timeout`).
fn wait_for_ready(port: u16, timeout: Duration) -> bool {
    let deadline = std::time::Instant::now();
    while deadline.elapsed() < timeout {
        if TcpStream::connect_timeout(
            &format!("127.0.0.1:{port}").parse().unwrap(),
            Duration::from_millis(200),
        )
        .is_ok()
        {
            return true;
        }
        thread::sleep(Duration::from_millis(100));
    }
    false
}

/// Start the stt-daemon binary as a child process.
fn start_daemon(port: u16, stt_server_url: &str) -> Child {
    let bin = env!("CARGO_BIN_EXE_stt-daemon");
    Command::new(bin)
        .env("STT_PORT", port.to_string())
        .env("STT_SERVER", stt_server_url)
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .expect("failed to spawn stt-daemon")
}

/// Send a simple HTTP GET request and return (status_code, body).
fn http_get(port: u16, path: &str) -> (u16, String) {
    let mut stream = TcpStream::connect(format!("127.0.0.1:{port}"))
        .expect("failed to connect to daemon");
    let req = format!("GET {path} HTTP/1.1\r\nHost: 127.0.0.1:{port}\r\nConnection: close\r\n\r\n");
    stream.write_all(req.as_bytes()).unwrap();
    let mut resp = String::new();
    stream.read_to_string(&mut resp).unwrap();

    let status_line = resp.lines().next().unwrap_or("");
    let status: u16 = status_line
        .split_whitespace()
        .nth(1)
        .and_then(|s| s.parse().ok())
        .unwrap_or(0);

    // body starts after the empty line separating headers
    let body = resp.split("\r\n\r\n").nth(1).unwrap_or("").to_string();
    (status, body)
}

/// A tiny mock HTTP server that records incoming POST bodies.
/// Listens on a free port and handles one request.
struct MockSttServer {
    port: u16,
    listener: Option<TcpListener>,
}

impl MockSttServer {
    fn start() -> Self {
        let listener = TcpListener::bind("127.0.0.1:0").expect("mock server bind");
        let port = listener.local_addr().unwrap().port();
        listener
            .set_nonblocking(true)
            .expect("nonblocking for mock");
        MockSttServer {
            port,
            listener: Some(listener),
        }
    }

    fn url(&self) -> String {
        format!("http://127.0.0.1:{}/stt", self.port)
    }

    /// Block until a request arrives, then return the raw request body bytes.
    /// Times out after 5 seconds.
    fn recv_one(&mut self) -> Option<Vec<u8>> {
        let listener = self.listener.take()?;
        let deadline = std::time::Instant::now();
        while deadline.elapsed() < Duration::from_secs(5) {
            if let Ok((mut stream, _)) = listener.accept() {
                let mut buf = [0u8; 65536];
                let n = stream.read(&mut buf).unwrap_or(0);
                // Extract body after \r\n\r\n
                if let Some(body_start) = buf[..n]
                    .windows(4)
                    .position(|w| w == b"\r\n\r\n")
                {
                    let body = buf[body_start + 4..n].to_vec();
                    // Send a mock STT response: JSON with transcribed text
                    let response = "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: 27\r\n\r\n{\"text\":\"hello world\"}\n";
                    let _ = stream.write_all(response.as_bytes());
                    return Some(body);
                }
                return Some(buf[..n].to_vec());
            }
            thread::sleep(Duration::from_millis(50));
        }
        None
    }
}

// ── tests ──────────────────────────────────────────────────────────────────

#[test]
fn toggle_flips_recording_state() {
    // RED: the stub always returns {"recording": false}, never toggling.
    // A correct implementation would flip between true and false on each GET /toggle.
    let port = free_port();
    let mut daemon = start_daemon(port, "http://127.0.0.1:9999/stt");
    assert!(wait_for_ready(port, Duration::from_secs(5)));

    // First toggle: should start recording
    let (status1, body1) = http_get(port, "/toggle");
    assert_eq!(status1, 200);
    let v1: serde_json::Value = serde_json::from_str(&body1).unwrap();
    let first_recording = v1["recording"].as_bool().unwrap();

    // Second toggle: should stop recording
    let (status2, body2) = http_get(port, "/toggle");
    assert_eq!(status2, 200);
    let v2: serde_json::Value = serde_json::from_str(&body2).unwrap();
    let second_recording = v2["recording"].as_bool().unwrap();

    daemon.kill().ok();
    daemon.wait().ok();

    // A toggle must change state: on→off or off→on.
    assert_ne!(
        first_recording, second_recording,
        "toggle must flip recording state: was {first_recording}, stayed {second_recording}"
    );
}

#[test]
fn recording_state_persists_between_requests() {
    // RED: stub always returns false. Correct impl remembers state across requests.
    let port = free_port();
    let mut daemon = start_daemon(port, "http://127.0.0.1:9999/stt");
    assert!(wait_for_ready(port, Duration::from_secs(5)));

    // Toggle on
    let (_, body_on) = http_get(port, "/toggle");
    let on: serde_json::Value = serde_json::from_str(&body_on).unwrap();

    daemon.kill().ok();
    daemon.wait().ok();

    // First toggle should report recording=true
    assert!(
        on["recording"].as_bool().unwrap_or(false),
        "after first toggle, recording state should be true, got: {body_on}"
    );
}

#[test]
fn toggle_idempotency_rapid_fire() {
    // RED: stub always returns false. Correct impl should handle rapid toggles
    // without crashing, each one flipping the state.
    let port = free_port();
    let mut daemon = start_daemon(port, "http://127.0.0.1:9999/stt");
    assert!(wait_for_ready(port, Duration::from_secs(5)));

    let mut states = Vec::new();
    for _ in 0..6 {
        let (status, body) = http_get(port, "/toggle");
        assert_eq!(status, 200, "rapid toggle should not crash");
        let v: serde_json::Value = serde_json::from_str(&body).unwrap();
        states.push(v["recording"].as_bool().unwrap_or(false));
    }

    daemon.kill().ok();
    daemon.wait().ok();

    // Each toggle should flip — consecutive entries should alternate.
    for i in 1..states.len() {
        assert_ne!(
            states[i - 1],
            states[i],
            "rapid toggle state {i}: expected flip, got {}→{}",
            states[i - 1],
            states[i]
        );
    }
}

#[test]
fn toggling_on_starts_mic_capture_and_streams_pcm_to_server_on_stop() {
    // RED — end-to-end behavior chain test.
    //
    // 1. Start a mock STT server.
    // 2. Start the daemon pointing at the mock server.
    // 3. Toggle on → daemon should begin mic capture + PCM buffering.
    // 4. Toggle off → daemon should POST buffered PCM to the mock STT server,
    //    receive transcribed text, and type it at the cursor.
    //
    // This test fails because the stub does none of these: no mic capture,
    // no PCM buffering, no HTTP POST to the STT server, no text typing.
    let daemon_port = free_port();
    let mut mock_stt = MockSttServer::start();
    let mut daemon = start_daemon(daemon_port, &mock_stt.url());
    assert!(wait_for_ready(daemon_port, Duration::from_secs(5)));

    // Toggle ON — start recording
    let (s1, b1) = http_get(daemon_port, "/toggle");
    assert_eq!(s1, 200, "toggle on should succeed");
    let on_resp: serde_json::Value =
        serde_json::from_str(&b1).expect("valid JSON from toggle on");
    assert!(
        on_resp["recording"].as_bool().unwrap_or(false),
        "toggle should report recording=true after starting"
    );

    // Let mic capture run briefly
    thread::sleep(Duration::from_millis(500));

    // Toggle OFF — stop recording, should trigger PCM POST to mock server
    let (s2, b2) = http_get(daemon_port, "/toggle");
    assert_eq!(s2, 200, "toggle off should succeed");
    let off_resp: serde_json::Value =
        serde_json::from_str(&b2).expect("valid JSON from toggle off");

    // Verify daemon sent PCM data to the mock STT server
    let pcm_body = mock_stt.recv_one();
    daemon.kill().ok();
    daemon.wait().ok();

    assert!(
        pcm_body.is_some(),
        "daemon should POST PCM data to STT server after recording stops"
    );
    let pcm_bytes = pcm_body.unwrap();
    assert!(
        !pcm_bytes.is_empty(),
        "PCM body should not be empty (mic captured some audio)"
    );

    // The toggle-off response should include transcription info from the server
    assert!(
        off_resp["recording"].as_bool() == Some(false),
        "toggle should report recording=false after stopping"
    );
}
