#!/bin/env bash

PARENT=`cat /proc/$PPID/comm`

# Options for powermenu
lock="    Lock"
logout="    Logout"
shutdown="    Poweroff"
reboot="    Reboot"
sleep="   Sleep"
hibernate="⚙  UEFI setup"

ROFI_CMD="rofi"
# Get answer from user via rofi
export COL=3
export LINES=2
export INPUT=false

if [[ $PARENT == "dwm" ]]; then
	ROFI_CMD="rofi -location 3 -no-fixed-num-lines"
	export COL=1
	unset LINES
	export YOFF=27px
fi

selected_option=$(
	echo "$lock
$logout
$sleep
$reboot
$shutdown
$hibernate" | $ROFI_CMD -dmenu -i -p "Power" \
		-font "Symbols Nerd Font 12" \
		-width "15" \
		-lines 4 -line-margin 3 -line-padding 10 -scrollbar-width "0"
)

if [ "$selected_option" == "$lock" ]; then
	XSECURELOCK_PASSWORD_PROMPT='asterisks' xsecurelock &
	sleep 1
	xset dpms force off
elif [ "$selected_option" == "$logout" ]; then
	$HOME/.bin/restartdwm
elif [ "$selected_option" == "$shutdown" ]; then
	systemctl poweroff
elif [ "$selected_option" == "$reboot" ]; then
	systemctl reboot
elif [ "$selected_option" == "$sleep" ]; then
	XSECURELOCK_PASSWORD_PROMPT='asterisks' xsecurelock &
	sleep 1
	systemctl suspend
elif [ "$selected_option" == "$hibernate" ]; then
	systemctl reboot --firmware-setup
else
	echo "No match"
fi
