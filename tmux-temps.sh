#!/bin/sh
# Print "CPU NN°C  NPU NN°C" for tmux status-right.
# zone0=soc-thermal, zone6=npu-thermal — values are in millidegrees C.
read -r cpu < /sys/class/thermal/thermal_zone0/temp
read -r npu < /sys/class/thermal/thermal_zone6/temp
printf 'CPU %d°C  NPU %d°C' $((cpu / 1000)) $((npu / 1000))
