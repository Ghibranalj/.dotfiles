#!/usr/bin/env bash

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

[[ "$(acpi | awk -F[,:] '{ print $2 }' | sed -n '2 p' | xargs)" == "Charging" ]]\
    && CHARGING="⚡" || CHARGING=""
BAT_PERC=$(acpi | awk -F[,:] '{ print $3 }' | sed -n '2 p' | xargs)
[[ $BAT_PERC -le 30  ]]\
    && BAT_ICON="🪫" || BAT_ICON="🔋"
BAT="$BAT_ICON$CHARGING$BAT_PERC"

TIME="$E $(date '+%a %d %b %H:%M')"
VOL=$(printf '\x01奄 %s' "$(pamixer --get-volume-human | xargs)")
BAT=$(printf '\x03 %s' "$BAT")
if [ $notif_num -gt 0 ]; then
    NOTIF=$(printf ' \x04🔔%s' $notif_num)
else
    NOTIF=""
fi

xsetroot -name "$TIME $VOL$BAT$NOTIF"
