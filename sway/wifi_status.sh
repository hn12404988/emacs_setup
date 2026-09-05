#!/bin/sh

# Find the first wireless interface operating as station
wlan=$(iwctl station list | sed 's/\x1b\[[0-9;]*[a-zA-Z]//g' | awk '$2 ~ /^(connected|disconnected|connecting)$/ {print $1; exit}')

if [ -z "$wlan" ]; then
    # Fallback to sysfs if iwctl station list was empty or failed
    wlan=$(for d in /sys/class/net/*/wireless; do [ -d "$d" ] && { basename "$(dirname "$d")"; break; }; done)
fi

if [ -z "$wlan" ]; then
    echo "No WiFi Device"
    exit 0
fi

# Get station details
show_out=$(iwctl station "$wlan" show | sed 's/\x1b\[[0-9;]*[a-zA-Z]//g')
state=$(echo "$show_out" | sed -n 's/^[[:space:]]*State[[:space:]]\+\(.*\)/\1/p' | xargs)

if [ "$state" = "connected" ]; then
    ssid=$(echo "$show_out" | sed -n 's/^[[:space:]]*Connected network[[:space:]]\+\(.*\)/\1/p' | xargs)
    rssi=$(echo "$show_out" | sed -n 's/^[[:space:]]*RSSI[[:space:]]\+\(.*\)/\1/p' | sed 's/\s*dBm//g' | xargs)
    
    if [ -n "$rssi" ]; then
        echo "  $ssid ($rssi dBm)"
    else
        echo "  $ssid"
    fi
elif [ "$state" = "connecting" ]; then
    echo "󰤨  Connecting..."
else
    echo "󰤮  Disconnected"
fi
