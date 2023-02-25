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
    printf ' \x08%s\x02%s' '🔉' "$(pamixer --get-volume-human)"
}

function battery() {

    BATTERY_STATUS=$(acpi | grep "Battery 0")

    if [[ "$BATTERY_STATUS" == *"unavailable"* ]]; then
      BATTERY_STATUS=$(acpi | grep "Battery 1")
    fi

    local CHARGING=$(acpi | awk '/Charging/ {print "⚡"}')
    # [[ "$(awk -F[,:] '{ print $2 }' <<<"$BATTERY_STATUS")" == "Charging" ]] &&
    #     CHARGING="⚡"

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

MUSIC_CACHE="$HOME/.cache/music.cache"
function music() {
    if [[ ! -f "$HOME/.config/spotify-tui/client.yml" ]]
    then
        echo -n "[spotify-tui not installed]"
        return
    fi

    touch $MUSIC_CACHE
    if [[ $((`date +%s` % 3)) -ne 0  ]]
    then
        MUSIC=$(cat $MUSIC_CACHE)
    else
        # MUSIC=$(spt pb -sf " %s %t - %a")
        MUSIC=""
        # echo $MUSIC > $MUSIC_CACHE
    fi

    printf '\x05%s' "$MUSIC"
}

function caffeine() {

    CAFFEINE_FILE=$HOME/.cache/caffeine

    local CAFFEINE_STATUS

    if [[ -f $CAFFEINE_FILE ]]; then
        CAFFEINE_STATUS="background='orange'"
    fi

    CAFFEINE="<span $CAFFEINE_STATUS >☕</span>"

    printf ' \x06%s' "$CAFFEINE"
}

function bluetooth() {
    local NUM_DEVICES=$(bluetoothctl devices Connected | wc -l)
    [[ "$NUM_DEVICES" == "0" ]] && NUM_DEVICES=""

    printf ' \x07%s' "<span color='#5255ba' font_size='115%' rise='-2pt' ></span> $NUM_DEVICES"
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
