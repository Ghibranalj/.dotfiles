#!/usr/bin/env bash


ICON=/usr/share/icons/Adwaita41/16x16/legacy/input-touchpad.png
declare -i ID
ID=$(xinput list | grep -Eio '(touchpad|glidepoint)\s*id=[0-9]{1,2}' | grep -Eo '[0-9]{1,2}')
declare -i STATE
STATE=$(xinput list-props "$ID" | grep 'Device Enabled' | awk '{print $4}')
if [ "$STATE" -eq 1 ]
then
    xinput disable "$ID"
    hcursor -x
    notify-send 'Touchpad Disabled' -i $ICON -u critical -e -r 7548
else
    xinput enable "$ID"
    hcursor -y
    notify-send 'Touchpad Enabled' -i $ICON -r 7548 -t 500
fi
