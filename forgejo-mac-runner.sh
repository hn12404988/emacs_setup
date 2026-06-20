#!/usr/bin/env bash
# forgejo-mac-runner.sh — quick on/off Forgejo Actions runner for a macOS machine.
#
# Registers this Mac as a *host-mode* runner for the self-hosted Forgejo on M6
# (tailnet-only) and starts/stops it on demand. Register-ONCE per machine;
# `down` keeps the registration so `up` reconnects instantly.
#
#   ./forgejo-mac-runner.sh up       # download + register (first time), then start
#   ./forgejo-mac-runner.sh down     # stop (keeps registration)
#   ./forgejo-mac-runner.sh status   # show state
#
# First `up` needs a registration token:
#   Forgejo (web) → avatar → Settings → Actions → Runners → "Create new runner"
#   → copy the token. Paste when asked, or pass it:
#     FORGEJO_RUNNER_TOKEN=xxxx ./forgejo-mac-runner.sh up
#
# Requirements: this Mac on the tailnet; Rust toolchain (cargo) for build jobs.

set -uo pipefail

# ---- Config (edit only if the Forgejo instance moves) ----
INSTANCE="https://nanopi-m6.tail2bfb5b.ts.net"
HOST="nanopi-m6.tail2bfb5b.ts.net"
LABELS="macos-14:host,macos:host,macos-latest:host"
MAGICDNS_RESOLVER="100.100.100.100"
# forgejo-runner ships NO macOS binaries (its releases are Linux-only). Gitea's
# act_runner — the project forgejo-runner was forked from — DOES ship darwin-arm64
# /amd64 and speaks the same Actions protocol, so it registers and runs against
# Forgejo. We use it as the macOS binary source.
# Trade-off: compatible, not official; could drift in a future major version.
RELEASES_API="https://gitea.com/api/v1/repos/gitea/act_runner/releases/latest"

# ---- State (all per-user, self-contained) ----
STATE_DIR="$HOME/.forgejo-runner"
BIN="$STATE_DIR/forgejo-runner"
RUNNER_FILE="$STATE_DIR/.runner"
PID_FILE="$STATE_DIR/daemon.pid"
LOG="$STATE_DIR/daemon.log"

# ---- pretty output ----
say()  { printf '\033[1;34m==>\033[0m %s\n' "$*"; }
ok()   { printf '\033[1;32m ✓ \033[0m%s\n' "$*"; }
warn() { printf '\033[1;33m ! \033[0m%s\n' "$*"; }
die()  { printf '\033[1;31m ✗ %s\033[0m\n' "$*" >&2; exit 1; }

[ "$(uname -s)" = "Darwin" ] || die "macOS only (this is $(uname -s)). Run it on your Mac."
mkdir -p "$STATE_DIR"

# ---- helpers ----
runner_name() {
  local n
  n="$(scutil --get LocalHostName 2>/dev/null || hostname -s 2>/dev/null || echo mac)"
  printf 'mac-%s' "$(printf '%s' "$n" | tr 'A-Z ' 'a-z-')"
}

is_running() {
  [ -f "$PID_FILE" ] || return 1
  kill -0 "$(cat "$PID_FILE" 2>/dev/null)" 2>/dev/null
}

reachable() { curl -fsS -o /dev/null --max-time 8 "$INSTANCE/" 2>/dev/null; }

ensure_binary() {
  if [ -x "$BIN" ]; then return 0; fi
  say "Downloading Gitea act_runner (macOS, Forgejo-compatible) ..."
  local arch url
  case "$(uname -m)" in
    arm64)  arch="darwin-arm64" ;;
    x86_64) arch="darwin-amd64" ;;
    *) die "Unsupported CPU: $(uname -m)" ;;
  esac
  # Pick the bare (uncompressed) binary whose URL ends in the arch, e.g.
  # gitea-runner-<ver>-darwin-arm64 — not its .xz / .sha256 siblings.
  url="$(curl -fsSL "$RELEASES_API" \
        | grep -oE '"browser_download_url":"[^"]*'"$arch"'"' \
        | grep -oE 'https[^"]*' | head -1)"
  [ -n "$url" ] || die "No $arch asset found in the latest act_runner release ($RELEASES_API)."
  curl -fSL -o "$BIN" "$url" || die "Download failed: $url"
  chmod +x "$BIN"
  # macOS quarantines downloaded binaries; strip it or Gatekeeper blocks execution.
  xattr -d com.apple.quarantine "$BIN" 2>/dev/null || true
  ok "Installed $("$BIN" --version 2>/dev/null | head -1)"
}

ensure_reachable() {
  if reachable; then ok "Forgejo reachable at $INSTANCE"; return 0; fi
  warn "Can't reach $INSTANCE — diagnosing ..."

  if command -v tailscale >/dev/null 2>&1 && ! tailscale status >/dev/null 2>&1; then
    die "Tailscale looks disconnected. Connect it and retry."
  fi

  # If Tailscale's resolver knows the name but the OS doesn't, it's the
  # macOS scoped-resolver gap (common with the Homebrew tailscaled).
  if command -v dig >/dev/null 2>&1 \
     && dig +short "@$MAGICDNS_RESOLVER" "$HOST" 2>/dev/null | grep -q '[0-9]'; then
    warn "Tailscale resolves $HOST, but macOS isn't routing *.ts.net to it."
    warn "Fix: /etc/resolver/ts.net -> $MAGICDNS_RESOLVER  (needs sudo)"
    printf "Apply this fix now? [y/N] "
    read -r ans
    case "$ans" in
      y|Y)
        sudo mkdir -p /etc/resolver
        printf 'nameserver %s\n' "$MAGICDNS_RESOLVER" | sudo tee /etc/resolver/ts.net >/dev/null
        sudo dscacheutil -flushcache 2>/dev/null || true
        sudo killall -HUP mDNSResponder 2>/dev/null || true
        ok "Added /etc/resolver/ts.net" ;;
      *) die "Skipped DNS fix — cannot continue without reachability." ;;
    esac
  fi

  reachable && { ok "Forgejo now reachable"; return 0; }
  die "Still can't reach $INSTANCE. Check Tailscale / network, then retry."
}

ensure_registered() {
  if [ -f "$RUNNER_FILE" ]; then ok "Already registered as $(runner_name)"; return 0; fi
  say "Registering this Mac as a Forgejo runner ..."
  local token="${FORGEJO_RUNNER_TOKEN:-}"
  if [ -z "$token" ]; then
    printf '\n  Need a registration token. In Forgejo (web):\n'
    printf '    avatar -> Settings -> Actions -> Runners -> "Create new runner" -> copy token\n'
    printf '    (or on M6:  sudo -u git forgejo actions generate-runner-token -c /etc/forgejo/app.ini)\n\n'
    printf "Paste registration token: "
    read -r token
  fi
  [ -n "$token" ] || die "No token provided."
  ( cd "$STATE_DIR" && "$BIN" register --no-interactive \
      --instance "$INSTANCE" --token "$token" \
      --name "$(runner_name)" --labels "$LABELS" ) \
    || die "Registration failed (see message above)."
  ok "Registered as $(runner_name)  [labels: $LABELS]"
}

check_rust() {
  if command -v cargo >/dev/null 2>&1; then
    ok "Rust present: $(cargo --version 2>/dev/null)"
  else
    warn "No 'cargo' on PATH — macOS build jobs will fail until you install Rust (https://rustup.rs)."
  fi
}

# ---- commands ----
cmd_up() {
  if is_running; then ok "Already up (pid $(cat "$PID_FILE"))."; exit 0; fi
  ensure_binary
  ensure_reachable
  ensure_registered
  check_rust
  say "Starting runner daemon ..."
  cd "$STATE_DIR" || die "cannot cd to $STATE_DIR"
  nohup "$BIN" daemon >"$LOG" 2>&1 &
  echo $! >"$PID_FILE"
  sleep 1
  if is_running; then
    ok "Runner UP (pid $(cat "$PID_FILE")).  log: $LOG"
    say "It shows as 'Idle' in Forgejo -> Settings -> Actions -> Runners."
  else
    warn "Daemon did not stay up. Last log lines:"; tail -n 20 "$LOG" 2>/dev/null
    die "Start failed."
  fi
}

cmd_down() {
  if ! is_running; then rm -f "$PID_FILE"; ok "Already down."; exit 0; fi
  local pid; pid="$(cat "$PID_FILE")"
  say "Stopping runner (pid $pid) ..."
  kill "$pid" 2>/dev/null || true
  for _ in 1 2 3 4 5; do is_running || break; sleep 1; done
  if is_running; then warn "Forcing stop."; kill -9 "$pid" 2>/dev/null || true; fi
  rm -f "$PID_FILE"
  ok "Runner DOWN (registration kept — 'up' reconnects instantly)."
}

cmd_status() {
  printf 'forgejo-mac-runner  [%s]\n' "$(runner_name)"
  printf '  instance : %s\n' "$INSTANCE"
  if reachable;            then printf '  reach    : \033[1;32mreachable\033[0m\n';
                           else printf '  reach    : \033[1;31mNOT reachable\033[0m\n'; fi
  if [ -f "$RUNNER_FILE" ]; then printf '  register : \033[1;32mregistered\033[0m\n';
                           else printf '  register : not registered\n'; fi
  if is_running;           then printf '  daemon   : \033[1;32mUP\033[0m (pid %s)\n' "$(cat "$PID_FILE")";
                           else printf '  daemon   : down\n'; fi
  if command -v cargo >/dev/null 2>&1; then printf '  rust     : %s\n' "$(cargo --version 2>/dev/null)";
                           else printf '  rust     : \033[1;33mmissing (build jobs will fail)\033[0m\n'; fi
  printf '  log      : %s\n' "$LOG"
}

usage() {
  cat <<EOF
forgejo-mac-runner.sh — quick on/off Forgejo Actions runner for this Mac

  up       download + register (first time), then start the runner
  down     stop the runner (keeps registration; 'up' reconnects instantly)
  status   show current state

First 'up' needs a registration token (Forgejo -> Settings -> Actions -> Runners
-> "Create new runner"). Paste when asked, or:  FORGEJO_RUNNER_TOKEN=xxx $0 up
EOF
}

case "${1:-}" in
  up)                 cmd_up ;;
  down)               cmd_down ;;
  status)             cmd_status ;;
  ""|-h|--help|help)  usage ;;
  *)                  usage; exit 1 ;;
esac
