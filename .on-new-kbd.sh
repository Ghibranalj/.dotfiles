#! /bin/sh
# from https://unix.stackexchange.com/questions/523635/prevent-keyboard-layout-reset-when-usb-keyboard-is-plugged-in#answer-523959


echo >&2 "$@"
event=$1 id=$2 type=$3

case "$event $type" in
'XIDeviceEnabled XISlaveKeyboard')
        touch /home/gibi/plugged_in
        xkbcomp /home/gibi/.xkbmap :1 &> /home/gibi/.keybind.log
esac