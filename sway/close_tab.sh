#!/bin/bash
# Close the focused sway "tab" (= workspace) and make sure the client really dies.
#
# ORDER MATTERS.
# sway's `kill` is not a signal. It is a polite "please close" request
# (xdg_toplevel.close); the app decides what to do with it.
#
# The old version marked the window, hid it in the scratchpad, and only then
# asked it to close. Apps that ask a question before closing draw that dialog
# INSIDE the window (chromium: "Leave site?" / "Close N tabs?"). A hidden window
# means an invisible question, so it could never be answered: the window stayed
# alive in the scratchpad forever, still holding all of its memory. That is
# expensive here, because chromium runs ONE process tree for every window it
# owns, so a single leaked window pins ~500 MB of shared browser/GPU/network
# processes on a 3.7 GB machine.
#
# Now: ask politely while the window is still visible, wait for it to actually
# disappear, escalate to real signals if it refuses, confirm the process is
# gone, and only then renumber the workspaces.

set -u

TICK=0.15        # poll interval, seconds
GRACE_TRIES=34   # ~5 s for a polite close before we start signalling
REAP_TRIES=27    # ~4 s for a process to exit after a signal

# sway's own pid, so we can tell apps sway launched from system services.
SWAY_PID=${SWAYSOCK##*sway-ipc.*.}; SWAY_PID=${SWAY_PID%.sock}
[ -n "${SWAY_PID//[0-9]/}" ] && SWAY_PID=$(pgrep -x sway | head -n1)

mark_present() { swaymsg -t get_marks | grep -q "\"$1\""; }

# Poll until the mark disappears. $1 mark, $2 tries. Returns 0 when gone.
wait_mark_gone() {
    local n=$2
    while [ "$n" -gt 0 ]; do
        mark_present "$1" || return 0
        sleep "$TICK"
        n=$((n - 1))
    done
    ! mark_present "$1"
}

# Poll until the pid disappears. $1 pid, $2 tries. Returns 0 when gone.
wait_pid_gone() {
    local n=$2
    while [ "$n" -gt 0 ]; do
        kill -0 "$1" 2>/dev/null || return 0
        sleep "$TICK"
        n=$((n - 1))
    done
    ! kill -0 "$1" 2>/dev/null
}

# May we signal this pid? Only if sway launched it as a tab.
#
# A footclient window reports the foot SERVER's pid, and that server lives under
# `systemd --user`, not under sway. Signalling it would close every terminal on
# the machine at once. Same idea for any other socket-activated service.
may_signal() {
    local ppid
    ppid=$(ps -o ppid= -p "$1" 2>/dev/null | tr -d ' ')
    [ -n "$ppid" ] && [ "$ppid" = "$SWAY_PID" ]
}

# Signal the whole process group when the process leads its own group -- that is
# how sway spawns children, and it catches chromium's zygotes, renderers, GPU and
# network helpers in one shot. Fall back to the single pid otherwise, so we can
# never take down sway's own group by accident.
signal_tree() {
    local pid=$1 sig=$2 pgid
    pgid=$(ps -o pgid= -p "$pid" 2>/dev/null | tr -d ' ')
    if [ -n "$pgid" ] && [ "$pgid" = "$pid" ]; then
        kill "-$sig" -- "-$pgid" 2>/dev/null
    else
        kill "-$sig" "$pid" 2>/dev/null
    fi
}

# --- what are we closing? ---------------------------------------------------

CURRENT=$(swaymsg -t get_workspaces | python3 -c '
import json, sys
print(next((w["num"] for w in json.load(sys.stdin) if w.get("focused")), 1))')

# Focused window: con id, pid, and how many sway windows share that pid.
read -r CON PID SHARED <<<"$(swaymsg -t get_tree | python3 -c '
import json, sys

wins, focused = [], None

def walk(n):
    global focused
    if n.get("pid") and (n.get("app_id") or n.get("window_properties")):
        wins.append(n["pid"])
        if n.get("focused"):
            focused = (n["id"], n["pid"])
    for c in n.get("nodes", []) + n.get("floating_nodes", []):
        walk(c)

walk(json.load(sys.stdin))
if focused is None:
    print("0 0 0")
else:
    cid, pid = focused
    print(cid, pid, wins.count(pid))
')"

# --- close it, and keep escalating until it is really gone -------------------

if [ "${CON:-0}" -gt 0 ]; then
    MARK="_close_$$"
    swaymsg "[con_id=$CON] mark --add $MARK" >/dev/null
    swaymsg "[con_id=$CON] kill" >/dev/null

    if ! wait_mark_gone "$MARK" "$GRACE_TRIES"; then
        # It refused. Usually a modal dialog it wants answered -- which the user
        # can now actually see, because we never hid the window.
        if [ "$SHARED" -gt 1 ] || ! may_signal "$PID"; then
            # Killing the process would take down windows the user never asked
            # to close. Hand the decision back instead of destroying their work.
            swaymsg "[con_mark=$MARK] focus" >/dev/null
            swaymsg "[con_mark=$MARK] unmark $MARK" >/dev/null
            swaynag -t warning -m "This window will not close, and process $PID cannot be killed safely (it owns $SHARED window(s) / is not a sway child). Answer its dialog, or close the others first." >/dev/null 2>&1 &
            exit 1
        fi
        signal_tree "$PID" TERM
        wait_mark_gone "$MARK" "$REAP_TRIES" || {
            signal_tree "$PID" KILL
            wait_mark_gone "$MARK" "$REAP_TRIES"
        }
    fi

    # The window is gone. If that was this process's last window, make sure the
    # process itself is gone too -- otherwise none of its memory comes back.
    if [ "$SHARED" -le 1 ] && [ "${PID:-0}" -gt 0 ] && may_signal "$PID"; then
        if ! wait_pid_gone "$PID" "$REAP_TRIES"; then
            signal_tree "$PID" TERM
            wait_pid_gone "$PID" "$REAP_TRIES" || signal_tree "$PID" KILL
        fi
    fi
fi

# --- collapse the tab, but only if the workspace really emptied --------------

REMAINING=$(swaymsg -t get_tree | python3 -c '
import json, sys
target, count = sys.argv[1], 0

def walk(n, num=None):
    global count
    if n.get("type") == "workspace":
        num = str(n.get("num"))
    if n.get("pid") and (n.get("app_id") or n.get("window_properties")) and num == target:
        count += 1
    for c in n.get("nodes", []) + n.get("floating_nodes", []):
        walk(c, num)

walk(json.load(sys.stdin))
print(count)' "$CURRENT")

# A split workspace that still holds another window is still a live tab.
[ "${REMAINING:-0}" -gt 0 ] && exit 0

# Decide which workspace to land on after this one goes away.
if [ "$CURRENT" -gt 1 ]; then
    TARGET=$((CURRENT - 1))
else
    TARGET=$(swaymsg -t get_workspaces | python3 -c '
import json, sys
n = [w["num"] for w in json.load(sys.stdin) if w["num"] > 1]
print(min(n) if n else 1)')
fi

# Land on TARGET; sway auto-removes the now-empty CURRENT once we leave it.
swaymsg "workspace number $TARGET" >/dev/null

# Shift higher-numbered workspaces down by one, ascending so names never collide.
swaymsg -t get_workspaces | python3 -c '
import json, sys
c = int(sys.argv[1])
for n in sorted(w["num"] for w in json.load(sys.stdin) if w["num"] > c):
    print(n)' "$CURRENT" | while read -r n; do
    swaymsg "rename workspace number $n to $((n - 1))" >/dev/null
done
