# ===========================================================================
# Machine setup guide — zshrc, tmux, Emacs, and tools
# ===========================================================================
#
# This repo contains config files for all of Willy's dev machines.
# Use this guide when setting up a new machine (especially a headless
# Armbian Linux box accessed via Tailscale SSH).
#
# FILES IN THIS REPO
# -------------------
#   zshrc_setup.sh      — This guide + zshrc template
#   .tmux.conf          — tmux config (Emacs-style keybindings)
#   init.el             — Emacs config
#   config              — Ghostty terminal config (macOS only)
#   aws-mfa-login.sh    — AWS MFA session credential helper
#   inline-suggestion/  — Emacs inline suggestion package
#
#
# ===========================================================================
# 1. PREREQUISITES
# ===========================================================================
#
# --- Armbian (Debian-based) ---
#
# FLASHING THE OS:
#   Use Armbian Imager (https://github.com/armbian/imager/releases)
#   to flash the SD card. Download the aarch64 .dmg for macOS.
#   Do NOT use raw dd — it is unreliable on some boards.
#
#   After first boot from SD card, move the OS to eMMC:
#     sudo armbian-install
#   Select option 2 (Boot from eMMC - system on eMMC), choose ext4.
#   After completion, power off, remove SD card, boot from eMMC.
#
# FIRST BOOT:
#   Armbian first boot: root / 1234. It will prompt you to change
#   the root password and create a regular user.
#
# SHELL:
#   Bash is fine on Debian headless — no need to install zsh.
#   Put shell functions and aliases in ~/.bashrc instead of ~/.zshrc.
#
# LOCALE AND TIMEZONE:
#   sudo locale-gen en_US.UTF-8
#   sudo update-locale LANG=en_US.UTF-8
#   sudo timedatectl set-timezone Asia/Taipei
#
# PACKAGES:
#   sudo apt update && sudo apt upgrade -y
#   sudo apt install -y emacs-nox tmux git curl python3 wget build-essential awscli
#
#   # Node.js — wait for SSD (node_modules are large)
#   # Deno — wait for SSD
#   # Docker — wait for SSD
#
#   # Install Google Cloud SDK
#   # https://cloud.google.com/sdk/docs/install#deb
#
#   # Install Tailscale
#   curl -fsSL https://tailscale.com/install.sh | sudo sh
#
#   # Install Check Point VPN — see section 8
#
# --- macOS ---
#
#   brew install emacs tmux
#
#   # tmux-256color terminfo (if missing):
#   infocmp tmux-256color || {
#     curl -LO https://invisible-island.net/datafiles/current/terminfo.src.gz
#     gunzip terminfo.src.gz
#     /usr/bin/tic -xe tmux-256color terminfo.src
#   }
#
#
# ===========================================================================
# 2. SECURITY HARDENING
# ===========================================================================
#
# --- SSH hardening ---
#
# Create /etc/ssh/sshd_config.d/hardening.conf:
#   echo -e "PasswordAuthentication no\nPermitRootLogin no\nPermitEmptyPasswords no" \
#     | sudo tee /etc/ssh/sshd_config.d/hardening.conf
#   sudo systemctl restart sshd
#
# Before doing this, make sure ssh-copy-id has been run from your
# primary machine so you don't lock yourself out.
#
# --- Passwordless sudo ---
#
# For the main user (so remote tools like Claude Code can operate):
#   sudo visudo -f /etc/sudoers.d/<username>
#   Add: <username> ALL=(ALL) NOPASSWD: ALL
#
# --- Firewall (Tailscale-only access) ---
#
#   sudo apt install -y ufw
#   sudo ufw default deny incoming
#   sudo ufw default allow outgoing
#   sudo ufw allow in on tailscale0
#   sudo ufw allow in on lo
#   sudo ufw --force enable
#
# This blocks ALL incoming traffic on the physical network interface.
# Only devices on your Tailscale network can reach the machine.
# Even if the router is compromised, attackers cannot SSH in.
#
# --- Tailscale ---
#
#   sudo tailscale set --operator=$USER
#   tailscale up
#
# DO NOT use `tailscale up --ssh`. Tailscale's SSH handler sets
# TERM=dumb which breaks tmux, colors, and OSC 52 clipboard.
# Instead, let regular OpenSSH handle SSH sessions over the
# Tailscale network interface — same security, full terminal support.
#
# --- Automatic security updates ---
#
#   sudo apt install -y unattended-upgrades
#   sudo dpkg-reconfigure -f noninteractive unattended-upgrades
#
# --- SSH config on the connecting machine ---
#
# Add to ~/.ssh/config on your primary machine:
#   Host m6
#     HostName <tailscale-ip>
#     User <username>
#     IdentityFile ~/.ssh/id_ed25519
#     SetEnv LC_CTYPE=en_US.UTF-8
#
#
# ===========================================================================
# 3. GHOSTTY TERMINFO (must do BEFORE first SSH from Ghostty)
# ===========================================================================
#
# Ghostty sets TERM=xterm-ghostty. When you SSH from a Ghostty terminal
# into a remote machine, the remote needs this terminfo — otherwise:
#   "missing or unsuitable terminal: xterm-ghostty"
#
# Push from the Ghostty machine to the remote:
#   infocmp -x xterm-ghostty | ssh <remote> tic -x -
#
# Or install locally on a machine that has Ghostty:
#   infocmp -x xterm-ghostty | tic -x -
#
#
# ===========================================================================
# 4. SHELL RC TEMPLATE
# ===========================================================================
#
# Copy the code block below into ~/.zshrc on the new machine.
# Fill in credentials yourself — they are marked with <PLACEHOLDER>.
#
# PLATFORM NOTES:
#   - PATH differs per platform; adjust as needed.
#   - DOCKER_HOST: macOS uses Colima socket; Linux uses native Docker
#     (no DOCKER_HOST needed, or use the default unix:///var/run/docker.sock).
#   - Check Point VPN: different CLI path per platform (see section 8).
#   - The aws-mfa-login.sh script uses `sed -i ''` (macOS). On Linux,
#     change to `sed -i` (no empty string argument).

# ---- PATH ----
# Adjust these paths based on what's installed on this machine.
# macOS example (Homebrew):
#   export PATH="$HOME/bin:/opt/homebrew/opt/node@18/bin:/opt/homebrew/opt/llvm/bin:$PATH"
# Linux example:
#   export PATH="$HOME/bin:$HOME/.local/bin:$HOME/.deno/bin:$PATH"

# ---- Google Cloud SDK ----
export GCLOUD_PROJECT="dev-chef"
# export GOOGLE_APPLICATION_CREDENTIALS="<path-to-service-account-key.json>"

# Google Cloud SDK path/completion (adjust path per machine)
# if [ -f '<path-to-gcloud>/path.zsh.inc' ]; then . '<path-to-gcloud>/path.zsh.inc'; fi
# if [ -f '<path-to-gcloud>/completion.zsh.inc' ]; then . '<path-to-gcloud>/completion.zsh.inc'; fi

# ---- Hugging Face cache ----
export HUGGINGFACE_HUB_CACHE="$HOME/hf_hub_cache/"
export HUGGINGFACE_ASSETS_CACHE="$HOME/hf_assets_cache/"

# ---- NPM ----
# export NPM_TOKEN="<your-npm-token>"
# export PG_NPM_TOKEN="<your-pg-npm-token>"

# ---- AWS ----
export AWS_REGION=us-east-1
# export AWS_ACCESS_KEY_ID="<from aws-mfa-login.sh>"
# export AWS_SECRET_ACCESS_KEY="<from aws-mfa-login.sh>"
# export AWS_SESSION_TOKEN="<from aws-mfa-login.sh>"

# ---- OpenAI ----
# export OPENAI_API_KEY="<your-key>"
# export OPENAI_ASSISTANT_ID="<your-assistant-id>"

# ---- Docker ----
# macOS (Colima):
#   export DOCKER_HOST="unix://$HOME/.colima/docker.sock"
# Linux: usually no DOCKER_HOST needed (native Docker uses default socket)

# ---- Amazon Selling Partner ----
# export SELLING_PARTNER_APP_CLIENT_ID="<your-client-id>"
# export SELLING_PARTNER_APP_CLIENT_SECRET="<your-client-secret>"

# ---- Internal services ----
# export PG_OPERATION_API_KEY='<your-key>'
# export AGENT_NOTES_KEY='<your-key>'
export AGENT_NOTES_HOST="https://d3vuzzwsmivxvg.cloudfront.net"

# ---- BigQuery ----
export BIG_QUERY_PROJECT_ID=nimble-net-279805
export BIG_QUERY_CLIENT_EMAIL=skulabtobigquery@nimble-net-279805.iam.gserviceaccount.com
export BIG_QUERY_DATASET_ID=aws_selling_partner
export BIG_QUERY_DATASET_LOCATION=US
export BIG_QUERY_DEFERRED_TABLE_ID=transactions_deferred
export BIG_QUERY_RELEASED_TABLE_ID=transactions_released
# export BIG_QUERY_PRIVATE_KEY="<your-private-key>"

# ---- DashScope ----
# export DASHSCOPE_API_KEY='<your-key>'

# ---- Deno ----
# . "$HOME/.deno/env"


# ===========================================================================
# 5. EMACS DAEMON + EMACSCLIENT (per-directory isolation)
# ===========================================================================
#
# WHAT THIS SOLVES
# ----------------
# By default, Emacs daemon mode uses a single global daemon. Every
# emacsclient instance connects to that one daemon, so ALL buffers are
# shared across every terminal/window. If you open project-A in one
# tmux pane and project-B in another, both see each other's buffers.
#
# This setup gives you per-directory isolation: each directory gets its
# own named Emacs daemon, so each emacsclient behaves like a standalone
# Emacs instance. Buffers in project-A stay in project-A.
#
#
# IMPORTANT: DO NOT USE A LAUNCHD/SYSTEMD EMACS DAEMON
# ----------------------------------------------------
# The e() function below manages daemon lifecycle automatically.
# A system-level daemon would conflict — it starts a single unnamed
# global daemon that fights with the per-directory named daemons.
#
# macOS: if you have ~/Library/LaunchAgents/gnu.emacs.daemon.plist, remove it.
# Linux: if you have a systemd emacs service, disable it.
#
#
# THE BIG GOTCHA: DAEMON STARTED INSIDE TMUX = BROKEN COLORS
# -----------------------------------------------------------
# When you type `e` inside a tmux pane, the daemon inherits tmux env
# vars (TMUX, TMUX_PANE, TERM=tmux-256color). Even though the daemon
# is headless, these pollute terminal handling — colors and rendering
# break completely when emacsclient later connects.
#
# FIX: start the daemon with `env -i` to wipe the environment, then
# pass back only what Emacs needs (HOME, PATH, TMPDIR).

# Fixed socket directory — never depends on TMPDIR.
EMACS_SOCK_DIR="/tmp/emacs$(id -u)"

_emacs_socket_path() {
  echo "$EMACS_SOCK_DIR/emacs-$(basename "$PWD")"
}

e() {
  local sock=$(_emacs_socket_path)
  if ! emacsclient -s "$sock" -e nil &>/dev/null; then
    env -i HOME="$HOME" PATH="$PATH" TMPDIR=/tmp emacs --daemon="$sock"
  fi
  if [ $# -eq 0 ]; then
    emacsclient -s "$sock" -nw --eval "(dired \"$(pwd)\")"
  else
    emacsclient -s "$sock" -nw "$@"
  fi
}

ekill() {
  local sock=$(_emacs_socket_path)
  emacsclient -s "$sock" -e "(kill-emacs)"
}

elist() {
  ps aux | grep -i 'emacs.*daemon' | grep -v grep
}

ekillall() {
  for s in "$EMACS_SOCK_DIR"/emacs-*; do
    [ -e "$s" ] && emacsclient -s "$s" -e "(kill-emacs)" 2>/dev/null
  done
}

export EDITOR='emacsclient -nw'

# ---- Check Point VPN CLI (see section 9 for install) ----
# macOS (trac):
#   alias pg='"/Library/Application Support/Checkpoint/Endpoint Connect/trac"'
#   Usage: pg info / pg connect / pg disconnect
# ARM64 Linux (snx-rs):
#   alias pg='sudo snx-rs -c ~/.config/snx-rs/config.conf'
#   alias pginfo='snx-rs -m info -s vpn.positivegrid.com:6443 -X true'
#   Usage: pg (connect, Ctrl+C to disconnect)

# ---- AWS MFA shortcut ----
# alias awsmfa='source ~/aws-mfa-login.sh'


# ===========================================================================
# 6. TMUX CONFIG
# ===========================================================================
#
# Copy .tmux.conf from this repo to ~/.tmux.conf on the new machine.
#
#   cp .tmux.conf ~/.tmux.conf
#
# IMPORTANT — PLATFORM ADJUSTMENT FOR CLIPBOARD:
#
# macOS:  send-keys -X copy-pipe-and-cancel "pbcopy"
# Linux headless (OSC 52 via SSH):
#   send-keys -X copy-selection-and-cancel
#
#   Do NOT use copy-pipe-and-cancel "" — it silently drops the copy.
#   Use copy-selection-and-cancel instead, which lets tmux's
#   `set -s set-clipboard on` handle OSC 52 passthrough to the
#   SSH client's terminal.
#
#   IMPORTANT: macOS Terminal.app does NOT support OSC 52.
#   You MUST use Ghostty (or iTerm2) for clipboard to work over SSH.
#
# Also add xterm-ghostty to terminal-overrides in .tmux.conf:
#   set -ag terminal-overrides ",xterm-ghostty:RGB"
#
# terminal-overrides in .tmux.conf:
#   - Each override matches the OUTER terminal (the one running tmux).
#   - "xterm-256color:RGB" covers most standard terminals.
#   - "xterm-ghostty:RGB" covers Ghostty.
#   - Do NOT add "tmux-256color:RGB" — that causes garbled rendering.
#   - If you SSH in from a different terminal, add its TERM value too.
#   - After changing overrides: tmux kill-server (source-file isn't enough).
#
#
# ===========================================================================
# 7. EMACS CONFIG
# ===========================================================================
#
# Copy init.el from this repo to ~/.emacs.d/init.el on the new machine.
#
#   mkdir -p ~/.emacs.d
#   cp init.el ~/.emacs.d/init.el
#
# First launch will auto-install packages (requires internet).
#
#
# ===========================================================================
# 8. AWS MFA LOGIN SCRIPT
# ===========================================================================
#
# Copy aws-mfa-login.sh to the new machine.
#
# IMPORTANT — LINUX ADJUSTMENTS:
#   1. sed syntax: change every `sed -i ''` to `sed -i` (no empty string)
#   2. Target file: change ZSHRC="$HOME/.zshrc" to ZSHRC="$HOME/.bashrc"
#      (if using bash instead of zsh)
#   3. Echo messages: update "~/.zshrc" references to "~/.bashrc"
#
# Also copy ~/.aws/credentials and ~/.aws/config from your primary machine.
# Add an alias: alias awsmfa='source ~/aws-mfa-login.sh'
#
# Usage:
#   awsmfa [profile]
#
# Copy SSH keys from your primary machine too:
#   scp ~/.ssh/id_* <new-machine>:~/.ssh/
#   scp ~/.ssh/config <new-machine>:~/.ssh/
# Remove machine-specific entries (e.g. Host m6, colima) from the
# remote copy of ~/.ssh/config.
#
#
# ===========================================================================
# 9. CHECK POINT VPN
# ===========================================================================
#
# Required by the company to connect to the corporate VPN.
#
# --- macOS ---
#
# Product: "Endpoint Security VPN" (GUI app from IT)
# CLI tool: /Library/Application Support/Checkpoint/Endpoint Connect/trac
# Already installed on the Mac mini.
#
# --- ARM64 Linux (NanoPi M6, Raspberry Pi, etc.) ---
#
# IMPORTANT: Check Point's official Endpoint Security does NOT support
# ARM64 Linux. Use snx-rs instead (open-source Rust client):
#   https://github.com/ancwrd1/snx-rs
#
# Download the ARM64 .deb from GitHub releases:
#   wget https://github.com/ancwrd1/snx-rs/releases/download/v5.2.4/snx-rs-v5.2.4-linux-arm64.deb
#   sudo dpkg -i snx-rs-v5.2.4-linux-arm64.deb
#   sudo apt install -f
#
# Create config file:
#   mkdir -p ~/.config/snx-rs
#   cat > ~/.config/snx-rs/config.conf << 'EOF'
#   server-name=vpn.positivegrid.com:6443
#   user-name=willie.chang
#   login-type=vpn
#   ignore-server-cert=true
#   tunnel-type=ipsec
#   EOF
#
# Add aliases to ~/.bashrc:
#   alias pg='sudo snx-rs -c ~/.config/snx-rs/config.conf'
#   alias pginfo='snx-rs -m info -s vpn.positivegrid.com:6443 -X true'
#
# Usage (different from macOS trac):
#   pg           — connect (prompts for password, runs in foreground)
#   Ctrl+C       — disconnect
#   pginfo       — show gateway info
#
# --- x86_64 Linux ---
#
# On x86_64, the official Check Point client may work:
#   Reference: https://support.checkpoint.com/results/sk/sk170198
#   Download .deb: https://support.checkpoint.com/results/download/142503
# Otherwise, use snx-rs as above (x86_64 builds also available).
#
# VPN site on Mac mini for reference:
#   vpn.positivegrid.com:6443 (auth: username-password, user: willie.chang)
#
# IMPORTANT — TAILSCALE + VPN ROUTING:
#   If the VPN uses full-tunnel mode, it may hijack all traffic and
#   break your Tailscale SSH session. Test carefully:
#     1. Connect VPN
#     2. Verify Tailscale SSH still works from another device
#   If it breaks, ask IT about split-tunnel configuration, or use
#   nohup/disown so the connect command survives a dropped session.
#
#
# ===========================================================================
# 10. STORAGE STRATEGY (eMMC vs SSD)
# ===========================================================================
#
# If the machine has both eMMC and an SSD (e.g. M.2 NVMe/SATA), split
# storage by purpose:
#
# eMMC — system tools (managed by apt):
#   emacs, tmux, git, gcc, etc. live in /usr/bin on the eMMC root
#   partition. These are small binaries, loaded once into RAM. Do NOT
#   try to move apt-managed packages to the SSD — it breaks package
#   management for no real gain.
#
# SSD — user data (I/O-heavy, large):
#   Mount the SSD as /home (or a subdirectory) so all user data lands
#   on it automatically. This includes:
#     - ~/.emacs.d/straight/  (500MB+, many small files — biggest
#       impact on Emacs daemon startup speed)
#     - Docker images and containers
#     - node_modules
#     - Project repos
#     - Python venvs
#     - Hugging Face cache
#
# WHY THIS MATTERS:
#   eMMC random 4K read is 10-50x slower than SSD. Emacs daemon
#   startup loads hundreds of small files from straight.el repos.
#   On eMMC this takes ~3s; on SSD it should be under 1s.
#   After the daemon is running, reconnecting is instant (0.003s).
#
#
# ===========================================================================
# 11. QUICK SETUP CHECKLIST (new Armbian headless machine)
# ===========================================================================
#
#   1.  Flash Armbian with Armbian Imager, boot, create user (section 1)
#   2.  Move OS from SD to eMMC: sudo armbian-install (section 1)
#   3.  Security hardening: SSH keys, firewall, Tailscale (section 2)
#   4.  Install Ghostty terminfo (section 3):
#         infocmp -x xterm-ghostty | ssh <new-machine> tic -x -
#   5.  If SSD present: mount as /home (section 10) BEFORE creating user data
#   6.  Install packages: emacs-nox tmux git build-essential awscli (section 1)
#   7.  Copy and fill in ~/.bashrc from the template (section 4 + 5)
#   8.  Copy .tmux.conf — change pbcopy → copy-selection-and-cancel,
#       add xterm-ghostty:RGB override (section 6)
#   9.  Copy init.el to ~/.emacs.d/init.el (section 7)
#   10. Copy SSH keys + config from primary machine (section 8)
#   11. Copy aws-mfa-login.sh — fix sed/bashrc targets (section 8)
#   12. Copy ~/.aws/credentials and ~/.aws/config
#   13. Install VPN: snx-rs for ARM64 (section 9)
#   14. Fill in credential env vars in ~/.bashrc
#   15. Verify: ssh m6, e, tmux copy mode, pg, awsmfa
#
#
# ===========================================================================
# 12. USAGE
# ===========================================================================
#
# e              — Open dired in current directory (auto-starts daemon)
# e file.txt     — Open file in current directory's daemon
# elist          — List all running Emacs daemons
# ekill          — Kill the daemon for the current directory
# ekillall       — Kill ALL running Emacs daemons
# pg info        — Show VPN status (macOS trac)
# pg connect     — Connect to VPN (macOS trac)
# pg disconnect  — Disconnect VPN (macOS trac)
# pg             — Connect to VPN (Linux snx-rs, Ctrl+C to disconnect)
# pginfo         — Show VPN gateway info (Linux snx-rs)
# awsmfa         — Refresh AWS MFA session credentials
#
# Closing emacsclient (the client frame, not the daemon):
#   C-x C-c      — Close frame with save prompts. Does NOT kill daemon.
#   C-x 5 0      — Close frame silently. Does NOT kill daemon.
#
# Both are safe. The daemon stays alive so reconnecting (`e` again)
# is instant — no startup delay, all buffers still there.
#
# To actually kill the daemon: `ekill` from the shell, or
# M-x kill-emacs from inside Emacs.
#
#
# ===========================================================================
# 13. TROUBLESHOOTING
# ===========================================================================
#
# "can't find socket" error:
#   A stale socket file may exist from a crashed daemon.
#     ls /tmp/emacs$(id -u)/
#     rm /tmp/emacs$(id -u)/emacs-<dirname>
#   Then try `e` again.
#
# Colors/rendering broken in tmux:
#   1. Make sure the daemon is started with `env -i` (see section 5).
#   2. Make sure ~/.tmux.conf has the correct terminal-overrides.
#   3. Kill tmux server fully and restart: tmux kill-server
#   4. Kill all daemons and restart: ekillall
#
# "Emacs server named X already running" error:
#   Kill the daemon process directly:
#     pkill -f 'emacs.*daemon.*<name>'
#   Remove the stale socket:
#     rm /tmp/emacs$(id -u)/emacs-<name>
#   Then try `e` again.
#
# "missing or unsuitable terminal: xterm-ghostty":
#   Push Ghostty terminfo to the remote machine (section 3).
#
# tmux copy mode doesn't copy to clipboard over SSH:
#   1. Make sure you're using Ghostty (not Terminal.app — no OSC 52).
#   2. tmux.conf must use `copy-selection-and-cancel` (not copy-pipe "").
#   3. tmux.conf must have `set -s set-clipboard on`.
#   4. Do NOT use Tailscale SSH (--ssh) — it sets TERM=dumb.
#      Use regular SSH over Tailscale network instead.
#
# TERM=dumb in SSH session:
#   Caused by Tailscale SSH handler. Disable it:
#     tailscale set --ssh=false --accept-risk=lose-ssh
#   Then reconnect via regular SSH. The firewall still protects you
#   (only tailscale0 interface allows incoming).
