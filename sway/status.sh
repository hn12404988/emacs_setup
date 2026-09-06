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
    printf '%s | CPU %s%% %s | MEM %s%% | BAT %s | %s\n' \
        "$WIFI" "$CPU" "$TEMP" "$MEM" "$BAT" "$(date +'%Y-%m-%d %X')"
done
