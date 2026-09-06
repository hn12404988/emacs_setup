#!/bin/bash

# CPU usage needs two samples of /proc/stat to compare, so the previous
# sample is kept in globals across loop iterations. That is why update_cpu
# assigns to $CPU instead of echoing it — calling it as $(update_cpu) would
# run it in a subshell and throw the saved sample away every time.
prev_total=0
prev_idle=0
CPU=0
MEM=0
TEMP="n/a"

update_cpu() {
    local cpu user nice system idle iowait irq softirq steal rest
    read -r cpu user nice system idle iowait irq softirq steal rest < /proc/stat
    # guest/guest_nice are already counted inside user/nice, so they are left out.
    local total=$((user + nice + system + idle + iowait + irq + softirq + steal))
    local idle_all=$((idle + iowait))
    local d_total=$((total - prev_total))
    local d_idle=$((idle_all - prev_idle))
    prev_total=$total
    prev_idle=$idle_all
    if [ "$d_total" -le 0 ]; then
        CPU=0
        return
    fi
    CPU=$(( (100 * (d_total - d_idle) + d_total / 2) / d_total ))
}

# MemAvailable, not MemFree: free memory ignores reclaimable cache and would
# report almost everything as used.
update_mem() {
    local key val total avail
    while read -r key val _; do
        case $key in
            MemTotal:) total=$val ;;
            MemAvailable:) avail=$val; break ;;
        esac
    done < /proc/meminfo
    if [ -z "$total" ] || [ "$total" -le 0 ]; then
        MEM=0
        return
    fi
    MEM=$(( (100 * (total - avail) + total / 2) / total ))
}

# CPU die temperature. hwmon numbers are handed out in probe order and can
# move between boots, so the sensor is located by driver name at startup
# instead of being hardcoded to a hwmonN path. k10temp is the AMD on-die
# sensor (what this machine has), coretemp the Intel equivalent. If neither
# is there, fall back to any hwmon input labelled "CPU" — on this board the
# embedded controller (cros_ec) exposes one, but it sits near the socket
# rather than on the die and so reads a few degrees low. Last resort is the
# ACPI thermal zone, which measures the board, not the CPU.
TEMP_FILE=
find_temp_file() {
    local want d f name label
    for want in k10temp coretemp; do
        for d in /sys/class/hwmon/hwmon*; do
            read -r name < "$d/name" 2>/dev/null || continue
            [ "$name" = "$want" ] || continue
            if [ -r "$d/temp1_input" ]; then
                TEMP_FILE=$d/temp1_input
                return
            fi
        done
    done
    for d in /sys/class/hwmon/hwmon*; do
        for f in "$d"/temp*_input; do
            read -r label < "${f%_input}_label" 2>/dev/null || continue
            if [ "$label" = "CPU" ] && [ -r "$f" ]; then
                TEMP_FILE=$f
                return
            fi
        done
    done
    if [ -r /sys/class/thermal/thermal_zone0/temp ]; then
        TEMP_FILE=/sys/class/thermal/thermal_zone0/temp
    fi
}
find_temp_file

# sysfs reports millidegrees C; round to whole degrees.
update_temp() {
    local milli
    if [ -z "$TEMP_FILE" ] || ! read -r milli < "$TEMP_FILE" 2>/dev/null; then
        TEMP="n/a"
        return
    fi
    TEMP="$(( (milli + 500) / 1000 ))°C"
}

# Which input method is typing right now. `fcitx5-remote -n` prints the unique
# name of the active engine ("keyboard-us", "chewing", ...).
#
# The binary is NOT resolved once at startup. It used to be, and that was a
# footgun: swaybar keeps this script alive for the whole session, so installing
# fcitx5 while sway was already running left the cached lookup empty and the bar
# stuck on EN until a reload. Exit code 127 (bash could not find the command)
# tells us the same thing per tick, and the $() subshell forks either way.
#
# Caveat: this shows the selected *engine*, which is what Ctrl+Space
# switches. Chewing's own Shift toggle flips to English inside the chewing
# engine, and fcitx5-remote does not report that, so the bar still reads 注音.
IM="EN"
update_im() {
    local name rc
    name=$(fcitx5-remote -n 2>/dev/null)
    rc=$?
    # 127: fcitx5 is not installed. Sway's bare xkb_layout is plain US English.
    if [ "$rc" -eq 127 ]; then
        IM="EN"
        return
    fi
    # Any other failure: installed, but the daemon is not answering. Say so,
    # otherwise a dead fcitx5 looks identical to "English is selected" and
    # Ctrl+Space seems broken for no visible reason.
    if [ "$rc" -ne 0 ]; then
        IM="EN (no IM)"
        return
    fi
    case $name in
        chewing)        IM="注音" ;;
        keyboard-us|"") IM="EN" ;;
        *)              IM="$name" ;;
    esac
}

# Prime the CPU counters, otherwise the first reading is the average since
# boot rather than the last second.
update_cpu

# The sleep is at the top of the loop, not the bottom, so every update_cpu
# call sits a full second after the previous one. With the sleep at the
# bottom the first sample landed a few ms after priming and printed a
# garbage spike. Cost is that the bar stays empty for the first second.
while true; do
    sleep 1
    BAT=$(acpi -b | grep -oE '[0-9]+%' | head -n1)
    WIFI=$(~/.config/sway/wifi_status.sh)
    update_cpu
    update_mem
    update_temp
    update_im
    # IM last so it lands in the very corner of the top-right bar.
    printf '%s | CPU %s%% %s | MEM %s%% | BAT %s | %s | %s\n' \
        "$WIFI" "$CPU" "$TEMP" "$MEM" "$BAT" "$(date +'%Y-%m-%d %X')" "$IM"
done
