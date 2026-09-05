#!/bin/bash

# CPU usage needs two samples of /proc/stat to compare, so the previous
# sample is kept in globals across loop iterations. That is why update_cpu
# assigns to $CPU instead of echoing it — calling it as $(update_cpu) would
# run it in a subshell and throw the saved sample away every time.
prev_total=0
prev_idle=0
CPU=0
MEM=0

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
    printf '%s | CPU %s%% | MEM %s%% | BAT %s | %s\n' \
        "$WIFI" "$CPU" "$MEM" "$BAT" "$(date +'%Y-%m-%d %X')"
done
