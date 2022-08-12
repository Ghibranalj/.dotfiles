#!/bin/env bash

# Options for powermenu
lock="    Lock"
logout="    Logout"
shutdown="    Poweroff"
reboot="    Reboot"
sleep="   Sleep"
hibernate="💤   Hibernate"

# Get answer from user via rofi
export COL=3
export LINES=2
selected_option=$(
	echo "$lock
$logout
$sleep
$reboot
$shutdown
$hibernate" | rofi -dmenu -i -p "Power" \
		-font "Symbols Nerd Font 12" \
		-width "15" \
		-lines 4 -line-margin 3 -line-padding 10 -scrollbar-width "0"
)

if [ "$selected_option" == "$lock" ]; then
	xset dpms force off
	sleep 1
	XSECURELOCK_PASSWORD_PROMPT='asterisks' xsecurelock
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
	XSECURELOCK_PASSWORD_PROMPT='asterisks' xsecurelock &
	sleep 1
	systemctl hibernate
else
	echo "No match"
fi
