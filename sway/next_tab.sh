#!/bin/bash
# Get focused workspace number
CURRENT=$(swaymsg -t get_workspaces | awk '
    /"num":/      { match($0, /[0-9]+/); n=substr($0, RSTART, RLENGTH) }
    /"focused": true/ { print n; exit }
')

NEW=$((CURRENT + 1))

# Shift workspaces >= NEW up by 1 (descending order avoids name collisions)
swaymsg -t get_workspaces | grep -o '"num": [0-9]*' | cut -d' ' -f2 \
    | sort -rn | awk -v n="$NEW" '$1 >= n' | while read -r w; do
    swaymsg "rename workspace number $w to $((w + 1))"
done

# Create the new tab right after CURRENT and launch the terminal
swaymsg "workspace number $NEW; exec footclient"
