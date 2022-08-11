#!/bin/env bash

# Options for powermenu
lock="    Lock"
logout="    Logout"
shutdown="    Poweroff"
reboot="    Reboot"
sleep="   Sleep"
#hibernate="💤   Hibernate"

# Get answer from user via rofi
export COL=6
export LINES=1
selected_option=$(
	echo "$lock
$logout
$sleep
$reboot
$shutdown" | rofi -dmenu -i -p "Power" \
		-font "Symbols Nerd Font 12" \
		-width "15" \
		-lines 4 -line-margin 3 -line-padding 10 -scrollbar-width "0"
)

if [ "$selected_option" == "$lock" ]; then
	xset dpms force off
elif [ "$selected_option" == "$logout" ]; then
	$HOME/.bin/restartdwm
elif [ "$selected_option" == "$shutdown" ]; then
	systemctl poweroff
elif [ "$selected_option" == "$reboot" ]; then
	systemctl reboot
elif [ "$selected_option" == "$sleep" ]; then
	systemctl suspend
else
	echo "No match"
fi
