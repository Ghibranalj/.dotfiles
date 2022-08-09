#!/usr/bin/env bash

while true; do

    H=$(date +%H)
    if [ $H -ge 0 -a $H -lt 6 ]; then
        E="💤"
    elif [ $H -ge 6 -a $H -lt 12 ]; then
        E="🐓"
    elif [ $H -ge 12 -a $H -lt 18 ]; then
        E="☀️"
    elif [ $H -ge 18 -a $H -lt 24 ]; then
        E="🌙"
    fi

    NOTIF_FILE="$HOME/.cache/dunst/dunst.log"
    if [[ -f $NOTIF_FILE ]]; then
        notif_num=$(cat $NOTIF_FILE | wc -l)
    else
        notif_num='0'
    fi

    TIME="$E $(date '+%a %d %b %H:%M')"
    VOL=$(printf '\x01奄 %s' $(pamixer --get-volume-human))
    BRIGHT=$(printf '\x02☀️ %s' $(brightness get))
    BAT=$(printf '\x03🔋 %s' $(upower -i /org/freedesktop/UPower/devices/battery_BAT0 | grep -E "percentage" | awk '{print $2}'))

    if [ $notif_num -gt 0 ]; then
        NOTIF=$(printf '\x04🔔%s' $notif_num)
    else
        NOTIF=""
    fi

    xsetroot -name "$TIME $VOL $BRIGHT $BAT $NOTIF"
done
