#!/usr/bin/env bash
# install-stt.sh — cross-platform install script for stt-daemon
#
# Detects OS and architecture, downloads stt-daemon binary from GitHub Releases,
# installs to ~/.local/bin, sets up systemd/LaunchAgent auto-start, and prints
# Ghostty keybind config.
#
# Usage:
#   ./install-stt.sh --help          show usage
#   ./install-stt.sh --dry-run       show what would be done
#   ./install-stt.sh --install       actually install
set -euo pipefail

REPO="hn12404988/emacs_setup"
INSTALL_DIR="$HOME/.local/bin"
BINARY_NAME="stt-daemon"

# ---- pretty output ----
say()  { printf '\033[1;34m==>\033[0m %s\n' "$*"; }
ok()   { printf '\033[1;32m ✓ \033[0m%s\n' "$*"; }
warn() { printf '\033[1;33m ! \033[0m%s\n' "$*"; }
die()  { printf '\033[1;31m ✗ %s\033[0m\n' "$*" >&2; exit 1; }

# ---- detection functions ----

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
    aarch64) echo "aarch64" ;;
    *)       die "Unsupported architecture: $(uname -m)" ;;
  esac
}

get_install_dir() {
  echo "$INSTALL_DIR"
}

# ---- download URL ----

download_url() {
  local os="$1" arch="$2"
  echo "https://github.com/${REPO}/releases/latest/download/${BINARY_NAME}-${os}-${arch}"
}

# ---- Ghostty config ----

print_ghostty_config() {
  cat <<'EOF'
# Ghostty keybind for stt-daemon toggle
keybind = super+shift+t=toggle:http://localhost:9876/toggle

# Alternative: if the above format isn't recognized by your Ghostty version, try:
# keybind = global:super+shift+t=unbind
# and use a separate hotkey daemon to POST http://localhost:9876/toggle
EOF
}

# ---- systemd unit (Linux) ----

generate_systemd_unit() {
  cat <<UNIT
[Unit]
Description=stt-daemon — speech-to-text service
After=network-online.target
Wants=network-online.target

[Service]
Type=simple
ExecStart=${INSTALL_DIR}/${BINARY_NAME}
Restart=on-failure
RestartSec=5

[Install]
WantedBy=default.target
UNIT
}

# ---- LaunchAgent plist (macOS) ----

generate_launchagent_plist() {
  cat <<PLIST
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
    <key>RunAtLoad</key>
    <true/>
    <key>KeepAlive</key>
    <true/>
</dict>
</plist>
PLIST
}

# ---- main ----

usage() {
  cat <<EOF
install-stt.sh — cross-platform install script for stt-daemon

  --help       show this help
  --dry-run    show what would be done without doing it
  --install    download and install stt-daemon, set up auto-start

Without --help, --dry-run, or --install, this script shows usage.
EOF
}

cmd_dry_run() {
  local os arch url
  os="$(detect_os)"
  arch="$(detect_arch)"
  url="$(download_url "$os" "$arch")"

  say "Dry run — nothing will be installed"
  echo "  OS:     $os"
  echo "  Arch:   $arch"
  echo "  URL:    $url"
  echo "  Install dir: $INSTALL_DIR"
  echo "  Binary: $INSTALL_DIR/$BINARY_NAME"

  echo ""
  echo "--- Ghostty config (to add to your Ghostty config file) ---"
  print_ghostty_config

  echo ""
  case "$os" in
    linux)
      echo "--- systemd user unit (would write to ~/.config/systemd/user/stt-daemon.service) ---"
      generate_systemd_unit
      echo ""
      echo "After install, enable with:"
      echo "  systemctl --user daemon-reload"
      echo "  systemctl --user enable stt-daemon --now"
      ;;
    darwin)
      echo "--- LaunchAgent plist (would write to ~/Library/LaunchAgents/com.stt-daemon.service.plist) ---"
      generate_launchagent_plist
      echo ""
      echo "After install, load with:"
      echo "  launchctl load ~/Library/LaunchAgents/com.stt-daemon.service.plist"
      ;;
  esac
}

cmd_install() {
  local os arch url dest
  os="$(detect_os)"
  arch="$(detect_arch)"
  url="$(download_url "$os" "$arch")"
  dest="$INSTALL_DIR/$BINARY_NAME"

  say "Installing stt-daemon for $os/$arch"

  # 1. Ensure install dir exists
  mkdir -p "$INSTALL_DIR"

  # 2. Download binary
  say "Downloading $url ..."
  curl -fSL -o "$dest" "$url" || die "Download failed: $url"
  chmod +x "$dest"
  ok "Installed $BINARY_NAME to $dest"

  # 3. Set up auto-start
  case "$os" in
    linux)
      local unit_dir="$HOME/.config/systemd/user"
      mkdir -p "$unit_dir"
      generate_systemd_unit > "$unit_dir/stt-daemon.service"
      ok "Wrote systemd user unit to $unit_dir/stt-daemon.service"
      say "To start:  systemctl --user daemon-reload && systemctl --user enable stt-daemon --now"
      ;;
    darwin)
      local agent_dir="$HOME/Library/LaunchAgents"
      mkdir -p "$agent_dir"
      generate_launchagent_plist > "$agent_dir/com.stt-daemon.service.plist"
      ok "Wrote LaunchAgent plist to $agent_dir/com.stt-daemon.service.plist"
      say "To start:  launchctl load $agent_dir/com.stt-daemon.service.plist"
      ;;
  esac

  # 4. Print Ghostty config
  echo ""
  say "Ghostty keybind config (add to your Ghostty config file):"
  print_ghostty_config

  ok "Install complete."
}

# Guard: when sourced, define functions only; when executed, parse args.
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
  case "${1:-}" in
    --help|-h|help)   usage ;;
    --dry-run)         cmd_dry_run ;;
    --install)          cmd_install ;;
    *)                  usage; exit 1 ;;
  esac
fi
