#! /bin/sh
# from https://unix.stackexchange.com/questions/523635/prevent-keyboard-layout-reset-when-usb-keyboard-is-plugged-in#answer-523959


#echo >&2 "$@"
event=$1 id=$2 type=$3


#echo $4 > ~/inputplug.log

if [[ "$4" == *"Basilisk X HyperSpeed"* ]]
then
#	echo "its caught" >> ~/inputplug.log
	exit
fi


case "$event $type" in
'XIDeviceEnabled XISlaveKeyboard')
        xkbcomp /home/gibi/.xkbmap $DISPLAY &> /home/gibi/.keybind.log
esac
