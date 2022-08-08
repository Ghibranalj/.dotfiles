#!/usr/bin/env bash

while true; do
    TIME="$(date '+%a %d %b %H:%M')"
    VOL=$(printf '\x01奄 %s' `pamixer --get-volume-human`)
    BRIGHT=$(printf '\x02☀️ %s' `brightness get`)
    BAT=$(printf '\x03🔋 %s' `upower -i /org/freedesktop/UPower/devices/battery_BAT0 | grep -E "percentage" | awk '{print $2}'`)
    xsetroot -name "$TIME  $VOL  $BRIGHT $BAT"
done;
