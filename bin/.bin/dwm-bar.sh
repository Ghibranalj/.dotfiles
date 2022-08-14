#!/usr/bin/env bash

function clock() {
    local H=$(date +%H)
    local E
    if [ $H -ge 0 -a $H -lt 6 ]; then
        E="💤"
    elif [ $H -ge 6 -a $H -lt 12 ]; then
        E="🐓"
    elif [ $H -ge 12 -a $H -lt 18 ]; then
        E="☀️"
    elif [ $H -ge 18 -a $H -lt 24 ]; then
        E="🌙"
    fi

    printf ' \x01%s' "$E $(date '+%H:%M %d/%m/%y')"
}

function vol() {
    printf ' \x08%s\x02%s ' '🔉' "$(pamixer --get-volume-human)"
}

function battery() {

    local BATTERY_NAME="Battery 1"
    BATTERY_STATUS=$(acpi | grep "$BATTERY_NAME")

    local CHARGING
    [[ "$(awk -F[,:] '{ print $2 }' <<<"$BATTERY_STATUS")" == "Charging" ]] &&
        CHARGING="⚡"

    local BAT_PERC=$(awk -F[,:] '{ print $3 }' <<<"$BATTERY_STATUS" | tr -d "%" | xargs)

    local BAT_ICON
    local COLOR
    if [[ $BAT_PERC -ge 95 ]]; then
        BAT_ICON=""
        COLOR="#bababa"
    elif [[ $BAT_PERC -ge 75 ]]; then
        BAT_ICON=""
        COLOR="#bababa"
    elif [[ $BAT_PERC -ge 50 ]]; then
        BAT_ICON=""
        COLOR="#FFFF00"
    elif [[ $BAT_PERC -ge 25 ]]; then
        COLOR="#FFA500"
        BAT_ICON=""
    else
        COLOR="#FF0000"
        BAT_ICON=""
    fi

    local BAT="<span color='$COLOR'>$BAT_ICON</span>$CHARGING $BAT_PERC%"
    printf ' \x03%s' "$BAT"
}

function notif() {

    NOTIF_FILE="$HOME/.cache/dunst/dunst.log"
    local notif_count
    if [[ -f $NOTIF_FILE ]]; then
        notif_count=$(cat $NOTIF_FILE | wc -l)
    else
        notif_count='0'
    fi

    if [[ $notif_count -gt 0 ]]; then
        printf ' \x04🔔 %s' $notif_count
    else
        echo -n ""
    fi
}

function music() {
    OUT="$($HOME/.bin/spotify-now -i "%artist %title" -e "EROR" -p "Paused")"
    OUT=${OUT//\&/&amp;}

    if [[ $OUT == "EROR" ]]; then
        echo -n ""
    else
        printf ' \x05🎶 %s' "$OUT"
    fi
}
function caffeine() {

    CAFFEINE_FILE=$HOME/.cache/caffeine

    local CAFFEINE_STATUS

    if [[ -f $CAFFEINE_FILE ]]; then
        CAFFEINE_STATUS="On "
    else
        CAFFEINE_STATUS="Off"
    fi

    CAFFEINE_ICON="☕"

    printf ' \x06%s' "$CAFFEINE_ICON$CAFFEINE_STATUS"
}

function bluetooth() {
    local NUM_DEVICES=$(bluetoothctl devices Connected | wc -l)
    [[ "$NUM_DEVICES" == "0" ]] && NUM_DEVICES=""

    printf '\x07%s' "<span color='#5255ba' font_stretch='expanded' font_size='large' rise='-1pt'></span> $NUM_DEVICES"
}

function puluseaudio(){
    printf '\x08%s' "<span font_size='x-large' rise='1pt'>☰</span> "
}

VOL=$(vol)
BAT=$(battery)
TIME=$(clock)
NOTIF=$(notif)
MUSIC=$(music)
CAFF=$(caffeine)
BLU=$(bluetooth)
# PULSE=$(puluseaudio)

echo -e "$MUSIC$TIME$VOL$BAT$CAFF$BLUE$BLU$NOTIF"
xsetroot -name "$MUSIC$TIME$VOL$PULSE$BAT$CAFF$BLUE$BLU$NOTIF"
