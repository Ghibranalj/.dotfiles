#!/usr/bin/env bash

    case $ROFI_INFO in
        "HIGH")
            $HOME/.bin/volume up --no-prompt
            ;;
        "LOW")
            $HOME/.bin/volume down --no-prompt
            ;;
        "MUTE")
            $HOME/.bin/volume mute --no-prompt
            ;;
    esac

VOL="$($HOME/.bin/volume get --no-prompt)"
N=$(bc <<< "($VOL / 3)")
for i in $(seq 1 $N); do
    BAR+="|"
done
COL="#ffffff"
if [[ $VOL -gt 75 ]]; then
    COL="#ff0000"
elif [[ $VOL -gt 50 ]]; then
    COL="#ffff00"
elif [[ $VOL -gt 25 ]]; then
    COL="#00ff00"
fi

echo -en "\0markup-rows\x1ftrue\n"
echo -en "\0no-custom\x1ftrue\n"
echo -en "\0Message\x1fVolume:$VOL% $BAR\n"
HIGH=/usr/share/icons/Adwaita41/scalable/status/audio-volume-high-symbolic.svg
LOW=/usr/share/icons/Adwaita41/scalable/status/audio-volume-low-symbolic.svg
# /usr/share/icons/Adwaita41/scalable/status/audio-volume-medium-symbolic.svg
MUTE=/usr/share/icons/Adwaita41/scalable/status/audio-volume-muted-symbolic.svg

echo -en "<span font_size='medium'>Down</span>\0icon\x1f$LOW\x1finfo\x1fLOW\n"
echo -en "<span font_size='medium'>Mute</span>\0icon\x1f$MUTE\x1finfo\x1fMUTE\n"
echo -en "<span font_size='medium'>Up</span>\0icon\x1f$HIGH\x1finfo\x1fHIGH\n"
