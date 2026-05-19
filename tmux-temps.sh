#!/bin/sh
# Print "CPU NN°C  NPU NN°C" for tmux status-right.
# zone0=soc-thermal, zone6=npu-thermal — values are in millidegrees C.
# Any reading > 60°C is wrapped in blinking red (tmux #[...] style markers).
read -r cpu < /sys/class/thermal/thermal_zone0/temp
read -r npu < /sys/class/thermal/thermal_zone6/temp
cpu=$((cpu / 1000))
npu=$((npu / 1000))

THRESHOLD=60
HOT_ON='#[fg=red,blink,bold]'
HOT_OFF='#[default]'

if [ "$cpu" -gt "$THRESHOLD" ]; then
  cpu_fmt="${HOT_ON}CPU ${cpu}°C${HOT_OFF}"
else
  cpu_fmt="CPU ${cpu}°C"
fi

if [ "$npu" -gt "$THRESHOLD" ]; then
  npu_fmt="${HOT_ON}NPU ${npu}°C${HOT_OFF}"
else
  npu_fmt="NPU ${npu}°C"
fi

printf '%s  %s' "$cpu_fmt" "$npu_fmt"
