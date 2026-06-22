#!/usr/bin/env bash
# pieces.sh — own every mechanical step of the pieces daemon so the agent does
# not have to. Idempotent: safe to run any number of times; it converges to
# "the pinned daemon is installed and healthy".
#
#   pieces.sh up                ensure the pinned daemon is installed and running
#   pieces.sh status            report whether it is up, and its version
#   pieces.sh send <payload>    ensure it, then POST the payload JSON; print the URL
#
# The version below is the SINGLE source that drives the download. CI guards
# that it equals pieces-daemon/Cargo.toml (see .forgejo/workflows/build-binaries.yml).
set -eu

PIECES_VERSION="0.2.1"
PIECES_PORT=8723
PIECES_DATA="$HOME/.local/share/pieces"
LOG="$PIECES_DATA/daemon.log"
REPO="hn12404988/emacs_setup"

# Progress/diagnostics go to stderr so stdout carries only the result (the URL).
say()  { printf '%s\n' "$*" >&2; }
warn() { printf 'pieces: %s\n' "$*" >&2; }
die()  { printf 'pieces: error: %s\n' "$*" >&2; exit 1; }

# Map uname to the release artifact naming and compute the install path.
# Sets globals: os, arch, BIN.
detect_arch() {
  os="$(uname -s)"; arch="$(uname -m)"
  case "$os" in
    Linux)  os=linux ;;
    Darwin) os=darwin ;;
    *) die "unsupported OS: $os (Linux and macOS only)" ;;
  esac
  case "$arch" in
    x86_64|amd64)  arch=x86_64 ;;
    aarch64|arm64) arch=arm64 ;;
    *) die "unsupported CPU: $arch" ;;
  esac
  BIN="$PIECES_DATA/$PIECES_VERSION/bin/pieces-$os-$arch"
}

# Echo the running daemon's version, or nothing if it is not reachable.
health_version() {
  curl -fsS "http://127.0.0.1:$PIECES_PORT/health" 2>/dev/null \
    | grep -o '"version"[[:space:]]*:[[:space:]]*"[^"]*"' \
    | sed -E 's/.*:[[:space:]]*"([^"]*)".*/\1/'
}

# Free the port (kills a stale-version daemon). fuser on Linux, lsof on macOS.
stop_port() {
  if command -v fuser >/dev/null 2>&1; then
    fuser -k "$PIECES_PORT"/tcp >/dev/null 2>&1 || true
    for _ in $(seq 1 20); do
      fuser -s "$PIECES_PORT"/tcp 2>/dev/null || break
      sleep 0.1
    done
  elif command -v lsof >/dev/null 2>&1; then
    pids="$(lsof -ti tcp:"$PIECES_PORT" 2>/dev/null || true)"
    [ -n "$pids" ] && kill $pids 2>/dev/null || true
    sleep 0.3
  fi
}

# Download the pinned binary if it is not already installed (atomic install).
ensure_binary() {
  [ -x "$BIN" ] && return 0
  url="https://github.com/$REPO/releases/download/v$PIECES_VERSION/pieces-$os-$arch"
  mkdir -p "$(dirname "$BIN")"
  say "Downloading pieces v$PIECES_VERSION ($os-$arch) ..."
  curl -fsSL "$url" -o "$BIN.partial" || die "download failed: $url"
  chmod +x "$BIN.partial"
  mv -f "$BIN.partial" "$BIN"
}

start_daemon() {
  mkdir -p "$PIECES_DATA"
  nohup "$BIN" --port "$PIECES_PORT" >> "$LOG" 2>&1 &
}

wait_health() {
  for _ in $(seq 1 50); do
    curl -fsS "http://127.0.0.1:$PIECES_PORT/health" >/dev/null 2>&1 && return 0
    sleep 0.1
  done
  warn "daemon did not become healthy — last log lines:"
  tail -n 20 "$LOG" 2>/dev/null >&2 || true
  die "start failed"
}

# The idempotent core: make sure the pinned-version daemon is up.
ensure_daemon() {
  detect_arch
  running="$(health_version || true)"
  if [ -n "$running" ]; then
    [ "$running" = "$PIECES_VERSION" ] && return 0
    say "Daemon on :$PIECES_PORT is v$running, want v$PIECES_VERSION — restarting."
    stop_port
  fi
  ensure_binary
  start_daemon
  wait_health
}

# Read or create this project's thread id (relative to the current directory).
ensure_thread() {
  file=".claude/pieces-thread"
  if [ ! -f "$file" ]; then
    mkdir -p .claude
    slug="$(basename "$PWD" | tr -c 'a-zA-Z0-9' '-' | sed 's/-\{1,\}/-/g; s/^-//; s/-$//')"
    rand="$(head -c3 /dev/urandom | od -An -tx1 | tr -d ' \n')"
    printf '%s\n' "${slug}-${rand}" > "$file"
  fi
  THREAD="$(cat "$file")"
}

cmd_up() {
  ensure_daemon
  say "✓ pieces v$PIECES_VERSION up"
  echo "http://127.0.0.1:$PIECES_PORT"
}

cmd_status() {
  running="$(health_version || true)"
  if [ -n "$running" ]; then
    echo "✓ pieces v$running up on http://127.0.0.1:$PIECES_PORT"
  else
    echo "✗ pieces daemon not running on port $PIECES_PORT"
    return 1
  fi
}

cmd_send() {
  payload="${1:-}"
  [ -n "$payload" ] || die "usage: pieces.sh send <payload.json>"
  [ -f "$payload" ] || die "payload file not found: $payload"
  ensure_daemon
  ensure_thread
  # No -f: capture the body even on a 4xx/5xx so we can report a bad payload.
  resp="$(curl -sS -w $'\n%{http_code}' \
            -X POST "http://127.0.0.1:$PIECES_PORT/messages" \
            -H "PIECES-THREAD: $THREAD" \
            -H "Content-Type: application/json" \
            --data @"$payload")" || die "could not reach daemon on :$PIECES_PORT"
  code="${resp##*$'\n'}"
  body="${resp%$'\n'*}"
  if [ "$code" != "200" ]; then
    warn "daemon returned HTTP $code:"
    printf '%s\n' "$body" >&2
    die "send failed (HTTP $code — often malformed JSON; fix and resend)"
  fi
  url="$(printf '%s' "$body" | sed -E 's/.*"url"[[:space:]]*:[[:space:]]*"([^"]*)".*/\1/')"
  { [ -n "$url" ] && [ "$url" != "$body" ]; } || die "unexpected response: $body"
  echo "http://127.0.0.1:$PIECES_PORT$url"
}

usage() {
  cat >&2 <<EOF
pieces.sh — manage the local pieces daemon (v$PIECES_VERSION)
  up                ensure the pinned daemon is installed and running
  status            report whether the daemon is up and its version
  send <payload>    ensure the daemon, then POST the payload JSON; prints the URL
EOF
}

main() {
  command -v curl >/dev/null 2>&1 || die "curl is required"
  cmd="${1:-}"
  case "$cmd" in
    up)     cmd_up ;;
    status) cmd_status ;;
    send)   shift; cmd_send "${1:-}" ;;
    ""|-h|--help|help) usage ;;
    *) die "unknown command: $cmd (use: up | status | send <payload.json>)" ;;
  esac
}

main "$@"
