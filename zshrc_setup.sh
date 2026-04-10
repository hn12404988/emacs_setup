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
# Armbian is Debian-based, so use apt. The default user is typically
# "root" on first boot — create a regular user if you haven't already.
#
#   sudo apt update && sudo apt upgrade -y
#   sudo apt install -y zsh emacs-nox tmux git curl python3 wget
#
#   # Set zsh as default shell
#   chsh -s $(which zsh)
#
#   # Install Node.js (via nvm or nodesource — pick your version)
#   curl -fsSL https://deb.nodesource.com/setup_18.x | sudo -E bash -
#   sudo apt install -y nodejs
#
#   # Install Deno
#   curl -fsSL https://deno.land/install.sh | sh
#
#   # Install Google Cloud SDK
#   # https://cloud.google.com/sdk/docs/install#deb
#
#   # Install AWS CLI
#   # https://docs.aws.amazon.com/cli/latest/userguide/getting-started-install.html
#
#   # Install Tailscale (for SSH access)
#   # https://tailscale.com/download/linux
#
#   # Install Check Point Endpoint Security (includes VPN) — see section 8
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
# 2. GHOSTTY TERMINFO (for Tailscale SSH from a Ghostty terminal)
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
# 3. ZSHRC TEMPLATE
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
# 4. EMACS DAEMON + EMACSCLIENT (per-directory isolation)
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

# ---- Check Point VPN CLI (see section 8 for install) ----
# macOS:
#   alias pg='"/Library/Application Support/Checkpoint/Endpoint Connect/trac"'
# Linux (adjust path after install — check where the package puts `trac`):
#   alias pg='/usr/bin/trac'    # or wherever it ends up; find with: dpkg -L <pkg> | grep trac
# Usage: pg info / pg connect / pg disconnect


# ===========================================================================
# 5. TMUX CONFIG
# ===========================================================================
#
# Copy .tmux.conf from this repo to ~/.tmux.conf on the new machine.
#
#   cp .tmux.conf ~/.tmux.conf
#
# IMPORTANT — PLATFORM ADJUSTMENT:
#
# The .tmux.conf uses "pbcopy" for clipboard in copy mode (macOS).
# On Linux, change the copy-pipe command:
#
#   macOS:  send-keys -X copy-pipe-and-cancel "pbcopy"
#   Linux:  send-keys -X copy-pipe-and-cancel "xclip -selection clipboard"
#
#   Or for a headless box accessed only via SSH, you can rely on OSC 52
#   (already enabled: `set -s set-clipboard on`), which forwards the
#   copied text to the SSH client's terminal clipboard. No xclip needed.
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
# 6. EMACS CONFIG
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
# 7. AWS MFA LOGIN SCRIPT
# ===========================================================================
#
# Copy aws-mfa-login.sh to the new machine.
#
# IMPORTANT — LINUX ADJUSTMENT:
# The script uses macOS sed syntax: sed -i '' ...
# On Linux, change every `sed -i ''` to `sed -i` (no empty string).
#
# Usage:
#   source ./aws-mfa-login.sh [profile]
#
# First set up base credentials in ~/.aws/credentials, then run the
# script to get temporary session credentials via MFA.
#
#
# ===========================================================================
# 8. CHECK POINT VPN
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
# --- Armbian / Linux ---
#
# Product: "Check Point Endpoint Security for Linux"
# Reference: https://support.checkpoint.com/results/sk/sk170198
#
# Download the .deb package (Armbian is Debian-based):
#   https://support.checkpoint.com/results/download/142503
#
# Install:
#   sudo dpkg -i Check_Point_Endpoint_Security_*.deb
#   sudo apt install -f    # fix any missing dependencies
#
# After install, find the CLI tool:
#   dpkg -L <package-name> | grep trac
#   # or search broadly:
#   find / -name "trac" -type f -executable 2>/dev/null
#
# Then set the alias in ~/.zshrc to wherever `trac` is found.
#
# NOTE: The exact CLI commands should match macOS (trac is cross-platform):
#   trac info                           — show VPN status and sites
#   trac connect -s <site>              — connect to VPN
#   trac connect -s <site> -u <user> -p <pass>
#   trac disconnect                     — disconnect
#   trac ver                            — show version
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
# 9. QUICK SETUP CHECKLIST (new Armbian headless machine)
# ===========================================================================
#
#   1.  Install prerequisites (section 1)
#   2.  Install Ghostty terminfo from your Ghostty machine (section 2):
#         infocmp -x xterm-ghostty | ssh <new-machine> tic -x -
#   3.  Copy and fill in ~/.zshrc from the template (section 3 + 4)
#   4.  Copy .tmux.conf — adjust pbcopy → xclip or rely on OSC 52 (section 5)
#   5.  Copy init.el to ~/.emacs.d/init.el (section 6)
#   6.  Copy aws-mfa-login.sh — fix sed -i syntax for Linux (section 7)
#   7.  Install Check Point Endpoint Security, set up VPN alias (section 8)
#   8.  Set up ~/.aws/credentials with base AWS keys
#   9.  Fill in all credential env vars in ~/.zshrc
#   10. Log out and back in (or source ~/.zshrc)
#   11. Verify: e, tmux, pg info
#
#
# ===========================================================================
# 10. USAGE
# ===========================================================================
#
# e              — Open dired in current directory (auto-starts daemon)
# e file.txt     — Open file in current directory's daemon
# elist          — List all running Emacs daemons
# ekill          — Kill the daemon for the current directory
# ekillall       — Kill ALL running Emacs daemons
# pg info        — Show VPN status
# pg connect     — Connect to VPN
# pg disconnect  — Disconnect VPN
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
# 11. TROUBLESHOOTING
# ===========================================================================
#
# "can't find socket" error:
#   A stale socket file may exist from a crashed daemon.
#     ls /tmp/emacs$(id -u)/
#     rm /tmp/emacs$(id -u)/emacs-<dirname>
#   Then try `e` again.
#
# Colors/rendering broken in tmux:
#   1. Make sure the daemon is started with `env -i` (see section 4).
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
