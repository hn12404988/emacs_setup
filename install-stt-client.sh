#!/usr/bin/env bash
# install-stt-client.sh — one-command install of the stt-daemon client
#
# Works on macOS and Linux.  Downloads the right binary from GitHub Releases,
# installs to ~/.local/bin, and sets up auto-start (LaunchAgent / systemd)
# with the STT server URL baked in.
#
# Usage:
#   ./install-stt-client.sh --server http://<rk3588-ip>:8080/stt
#   ./install-stt-client.sh --server http://<rk3588-ip>:8080/stt --dry-run
set -euo pipefail

REPO="hn12404988/emacs_setup"
INSTALL_DIR="$HOME/.local/bin"
BINARY_NAME="stt-daemon"
STT_SERVER=""

# ---- pretty output ----
say()  { printf '\033[1;34m==>\033[0m %s\n' "$*"; }
ok()   { printf '\033[1;32m ✓ \033[0m%s\n' "$*"; }
warn() { printf '\033[1;33m ! \033[0m%s\n' "$*"; }
die()  { printf '\033[1;31m ✗ %s\033[0m\n' "$*" >&2; exit 1; }

# ---- detection ----
detect_os() {
  case "$(uname -s)" in
    Linux)  echo "linux" ;;
    Darwin) echo "darwin" ;;
    *)      die "Unsupported OS: $(uname -s)" ;;
  esac
}

detect_arch() {
  case "$(uname -m)" in
    x86_64)  echo "x86_64" ;;
    arm64)   echo "arm64" ;;
    aarch64) echo "arm64" ;;
    *)       die "Unsupported architecture: $(uname -m)" ;;
  esac
}

download_url() {
  local os="$1" arch="$2"
  echo "https://github.com/${REPO}/releases/latest/download/${BINARY_NAME}-${os}-${arch}"
}

# ---- Ghostty config ----
print_ghostty_config() {
  if [ "$(detect_os)" = "darwin" ]; then
    echo "  Ghostty config: ~/Library/Application Support/com.mitchellh.ghostty/config"
  else
    echo "  Ghostty config: ~/.config/ghostty/config"
  fi
  echo ""
  echo 'keybind = super+shift+t=sh -c "curl -s http://localhost:9876/toggle"'
}

# ---- systemd unit (Linux) ----
write_systemd_unit() {
  local unit_dir="$HOME/.config/systemd/user"
  mkdir -p "$unit_dir"
  cat > "$unit_dir/stt-daemon.service" <<UNIT
[Unit]
Description=stt-daemon — speech-to-text client
After=network-online.target
Wants=network-online.target

[Service]
Type=simple
ExecStart=${INSTALL_DIR}/${BINARY_NAME}
Environment=STT_SERVER=${STT_SERVER}
Restart=on-failure
RestartSec=5

[Install]
WantedBy=default.target
UNIT
  ok "Wrote systemd unit to $unit_dir/stt-daemon.service"
}

# ---- LaunchAgent plist (macOS) ----
write_launchagent_plist() {
  local agent_dir="$HOME/Library/LaunchAgents"
  mkdir -p "$agent_dir"
  cat > "$agent_dir/com.stt-daemon.service.plist" <<PLIST
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN"
  "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>Label</key>
    <string>com.stt-daemon.service</string>
    <key>ProgramArguments</key>
    <array>
        <string>${INSTALL_DIR}/${BINARY_NAME}</string>
    </array>
    <key>EnvironmentVariables</key>
    <dict>
        <key>STT_SERVER</key>
        <string>${STT_SERVER}</string>
    </dict>
    <key>RunAtLoad</key>
    <true/>
    <key>KeepAlive</key>
    <true/>
</dict>
</plist>
PLIST
  ok "Wrote LaunchAgent to $agent_dir/com.stt-daemon.service.plist"
}

# ---- usage ----
usage() {
  cat <<EOF
install-stt-client.sh — one-command stt-daemon client install

  --server <url>   STT server URL (required)
                   e.g. http://10.0.0.5:8080/stt
  --dry-run        Show what would be done, don't do it
  --help           Show this help

Example:
  ./install-stt-client.sh --server http://100.100.100.100:8080/stt
EOF
}

# ---- dry-run ----
cmd_dry_run() {
  local os arch url
  os="$(detect_os)"
  arch="$(detect_arch)"
  url="$(download_url "$os" "$arch")"

  say "Dry run — nothing will be installed"
  echo "  OS:          $os"
  echo "  Arch:        $arch"
  echo "  Binary URL:  $url"
  echo "  Install to:  $INSTALL_DIR/$BINARY_NAME"
  echo "  STT server:  $STT_SERVER"

  echo ""
  echo "--- Ghostty keybind (add to your config) ---"
  print_ghostty_config

  echo ""
  case "$os" in
    linux)
      echo "--- systemd user unit (would be written) ---"
      echo "  Environment=STT_SERVER=$STT_SERVER"
      echo ""
      echo "After install:"
      echo "  systemctl --user daemon-reload"
      echo "  systemctl --user enable stt-daemon --now"
      ;;
    darwin)
      echo "--- LaunchAgent plist (would be written) ---"
      echo "  EnvironmentVariables.STT_SERVER = $STT_SERVER"
      echo ""
      echo "After install:"
      echo "  launchctl load ~/Library/LaunchAgents/com.stt-daemon.service.plist"
      ;;
  esac
}

# ---- install ----
cmd_install() {
  local os arch url dest
  os="$(detect_os)"
  arch="$(detect_arch)"
  url="$(download_url "$os" "$arch")"
  dest="$INSTALL_DIR/$BINARY_NAME"

  say "Installing stt-daemon client for $os/$arch"
  echo "  Server: $STT_SERVER"

  # 1. Ensure install dir exists
  mkdir -p "$INSTALL_DIR"

  # 2. Download binary
  say "Downloading $url ..."
  curl -fSL --progress-bar -o "$dest" "$url" || die "Download failed: $url"
  chmod +x "$dest"
  ok "Installed $BINARY_NAME to $dest ($(du -h "$dest" | cut -f1))"

  # 3. Set up auto-start
  case "$os" in
    linux)
      write_systemd_unit
      say "Enable and start:"
      echo "  systemctl --user daemon-reload"
      echo "  systemctl --user enable stt-daemon --now"
      ;;
    darwin)
      write_launchagent_plist
      say "Load the LaunchAgent:"
      echo "  launchctl load ~/Library/LaunchAgents/com.stt-daemon.service.plist"
      ;;
  esac

  # 4. Show Ghostty config
  echo ""
  say "Ghostty keybind — add this to your config:"
  print_ghostty_config

  echo ""
  ok "Done.  Press the keybind, speak, press again → text appears at cursor."
}

# ---- main ----
STT_SERVER=""
DRY_RUN=false

while [[ $# -gt 0 ]]; do
  case "$1" in
    --server)
      STT_SERVER="${2:-}"
      shift 2
      ;;
    --dry-run)
      DRY_RUN=true
      shift
      ;;
    --help|-h|help)
      usage
      exit 0
      ;;
    *)
      die "Unknown flag: $1 (use --help)"
      ;;
  esac
done

if [ -z "$STT_SERVER" ]; then
  die "--server <url> is required.  e.g. --server http://100.100.100.100:8080/stt"
fi

if ! [[ "$STT_SERVER" =~ ^https?:// ]]; then
  die "STT_SERVER must start with http:// or https:// (got: $STT_SERVER)"
fi

if $DRY_RUN; then
  cmd_dry_run
else
  cmd_install
fi
