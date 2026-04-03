# ===========================================================================
# Emacs + tmux + emacsclient setup guide (macOS)
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
# ===========================================================================
# PREREQUISITES
# ===========================================================================
#
# 1. Install Emacs:
#      brew install emacs
#
# 2. Install tmux:
#      brew install tmux
#
# 3. Make sure tmux-256color terminfo is available:
#      infocmp tmux-256color
#    If that fails, install it:
#      curl -LO https://invisible-island.net/datafiles/current/terminfo.src.gz
#      gunzip terminfo.src.gz
#      /usr/bin/tic -xe tmux-256color terminfo.src
#
#
# ===========================================================================
# IMPORTANT: DO NOT USE A LAUNCHD EMACS DAEMON
# ===========================================================================
#
# You might find guides telling you to create a launch agent at:
#   ~/Library/LaunchAgents/gnu.emacs.daemon.plist
#
# DO NOT DO THIS. The e() function below manages daemon lifecycle
# automatically. A launchd daemon would conflict — it starts a single
# unnamed global daemon that fights with the per-directory named daemons.
#
# If you already have one, remove it:
#   launchctl unload ~/Library/LaunchAgents/gnu.emacs.daemon.plist
#   rm ~/Library/LaunchAgents/gnu.emacs.daemon.plist
#
#
# ===========================================================================
# TMUX CONFIG (~/.tmux.conf)
# ===========================================================================
#
# You need these two lines for true color (24-bit) support in Emacs:
#
#   set -g default-terminal "tmux-256color"
#   set -ag terminal-overrides ",xterm-256color:RGB"
#
# CRITICAL NOTES:
#
#   - "terminal-overrides" matches the OUTER terminal (the terminal
#     emulator that is running tmux), NOT the TERM used inside tmux.
#     Your outer terminal is typically "xterm-256color".
#
#   - Do NOT add "tmux-256color:RGB" to terminal-overrides.
#     That pattern would only match if tmux were running inside another
#     tmux (TERM=tmux-256color outside). Adding it causes garbled output
#     and broken rendering.
#
#   - If your terminal emulator uses a different TERM (e.g. "alacritty"),
#     change the pattern accordingly: ",alacritty:RGB"
#
#   - After changing terminal-overrides, you MUST kill the tmux server
#     and start fresh. Just sourcing the config is NOT enough:
#       tmux kill-server
#
#
# ===========================================================================
# THE BIG GOTCHA: DAEMON STARTED INSIDE TMUX = BROKEN COLORS
# ===========================================================================
#
# This is the hardest bug to diagnose. Here's the full story:
#
# When you type `e` inside a tmux pane, the e() function starts an
# Emacs daemon. That daemon process inherits the current shell's
# environment, which inside tmux includes:
#
#   TMUX=/tmp/tmux-501/default,...   (tmux session info)
#   TMUX_PANE=%0                     (pane identifier)
#   TERM=tmux-256color               (tmux's terminal type)
#   ... and other tmux-specific variables
#
# Even though the daemon is "headless" (no display), these inherited
# tmux variables pollute how Emacs initializes terminal handling.
# When emacsclient later connects and creates a frame, the colors
# and rendering are completely wrong — washed out, missing colors,
# garbled display.
#
# This does NOT happen when the daemon is started outside tmux (e.g.
# by launchd or from a plain terminal), because those environments
# don't have TMUX/TMUX_PANE variables.
#
# THE FIX: start the daemon with `env -i` to wipe the environment
# clean, then pass back ONLY the variables Emacs actually needs:
#
#   env -i HOME="$HOME" PATH="$PATH" TMPDIR=/tmp emacs --daemon="$sock"
#
# Why each variable:
#
#   HOME   — Emacs needs this to find ~/.emacs.d/init.el and packages.
#            Without it, Emacs can't load your configuration.
#
#   PATH   — Emacs needs this to find external programs: LSP servers
#            (rust-analyzer, pyright, deno), git, aspell, pandoc, etc.
#            Without it, LSP, flycheck, magit, and spell-check all break.
#
#   TMPDIR — We hardcode TMPDIR=/tmp so the daemon always creates its
#            socket under /tmp/emacs<UID>/. This is critical for remote
#            access: on macOS, the local TMPDIR is /var/folders/37/5h4y.../T/,
#            but SSH sessions (e.g. Tailscale SSH) may have a different or
#            empty TMPDIR. By always using /tmp, both local and remote
#            sessions find sockets in the same place, so `ekill` and
#            `ekillall` work from anywhere.
#
#
# ===========================================================================
# ZSHRC SNIPPET — paste the code below into ~/.zshrc
# ===========================================================================

# Emacs daemon + emacsclient (one daemon per directory for isolation)
#
# How it works:
#   - Socket name is derived from the current directory basename.
#     e.g. in ~/projects/my-app, the socket is "emacs-my-app".
#   - If no daemon exists for this directory, one is started automatically.
#   - If a daemon already exists, emacsclient just connects to it.
#   - Two terminals in the SAME directory share one daemon (and buffers).
#   - Two terminals in DIFFERENT directories get separate daemons.
#   - Sockets always live under /tmp/emacs<UID>/ so that local and
#     remote (e.g. Tailscale SSH) sessions can manage the same daemons.

# Fixed socket directory — never depends on TMPDIR.
EMACS_SOCK_DIR="/tmp/emacs$(id -u)"

_emacs_socket_path() {
  echo "$EMACS_SOCK_DIR/emacs-$(basename "$PWD")"
}

e() {
  local sock=$(_emacs_socket_path)

  # Check if a daemon with this socket name is already running.
  # `emacsclient -e nil` is a lightweight ping — returns "nil" if
  # the server is reachable, errors out if not.
  if ! emacsclient -s "$sock" -e nil &>/dev/null; then
    # No daemon running for this directory. Start one.
    # env -i wipes inherited environment (especially tmux vars that
    # break colors). We pass back only what Emacs needs.
    # TMPDIR=/tmp ensures the socket lands in our fixed EMACS_SOCK_DIR.
    env -i HOME="$HOME" PATH="$PATH" TMPDIR=/tmp emacs --daemon="$sock"
  fi

  # Connect to the daemon.
  #   -s "$sock"  — use the named socket (full path) for this directory
  #   -nw         — open in the terminal (no GUI window)
  if [ $# -eq 0 ]; then
    # No arguments: open dired (file browser) at current directory
    emacsclient -s "$sock" -nw --eval "(dired \"$(pwd)\")"
  else
    # Arguments given: open the specified file(s)
    emacsclient -s "$sock" -nw "$@"
  fi
}

# Kill the daemon for the current directory only.
ekill() {
  local sock=$(_emacs_socket_path)
  emacsclient -s "$sock" -e "(kill-emacs)"
}

# List all running Emacs daemons.
elist() {
  ps aux | grep -i 'emacs.*daemon' | grep -v grep
}

# Nuclear option: kill ALL Emacs daemons.
ekillall() {
  for s in "$EMACS_SOCK_DIR"/emacs-*; do
    [ -e "$s" ] && emacsclient -s "$s" -e "(kill-emacs)" 2>/dev/null
  done
}

export EDITOR='emacsclient -nw'


# ===========================================================================
# USAGE
# ===========================================================================
#
# e              — Open dired in current directory (auto-starts daemon)
# e file.txt     — Open file in current directory's daemon
# elist          — List all running Emacs daemons
# ekill          — Kill the daemon for the current directory
# ekillall       — Kill ALL running Emacs daemons
#
# Closing emacsclient (the client frame, not the daemon):
#   C-x C-c      — Close frame with save prompts. Does NOT kill daemon.
#   C-x 5 0      — Close frame silently. Does NOT kill daemon.
#
# Both are safe. The daemon stays alive so reconnecting (running `e`
# again) is instant — no startup delay, all buffers still there.
#
# To actually kill the daemon: use `ekill` from the shell, or
# M-x kill-emacs from inside Emacs.
#
#
# ===========================================================================
# TROUBLESHOOTING
# ===========================================================================
#
# "can't find socket" error:
#   A stale socket file may exist from a crashed daemon.
#   Find and remove it:
#     ls $TMPDIR/emacs$(id -u)/
#     rm $TMPDIR/emacs$(id -u)/emacs-<dirname>
#   Then try `e` again.
#
# Colors/rendering broken in tmux:
#   1. Make sure the daemon is started with `env -i` (see above).
#   2. Make sure ~/.tmux.conf has the correct terminal-overrides
#      (see TMUX CONFIG section above).
#   3. Kill tmux server fully and restart: tmux kill-server
#   4. Kill all daemons and restart: ekillall
#
# "Emacs server named X already running" error:
#   A daemon is running but the socket file is stale/mismatched.
#   Kill the daemon process directly:
#     pkill -f 'emacs.*daemon.*<name>'
#   Remove the stale socket:
#     rm $TMPDIR/emacs$(id -u)/emacs-<name>
#   Then try `e` again.
