#!/bin/env bash

# Options for powermenu
lock="    Lock"
logout="    Logout"
shutdown="    Poweroff"
reboot="    Reboot"
sleep="   Sleep"
#hibernate="💤   Hibernate"

# Get answer from user via rofi
selected_option=$(
	echo "$lock
$logout
$sleep
$reboot
$shutdown" | rofi -dmenu -i -p "Power" \
		-config "~/.config/rofi/power.rasi" \
		-font "Symbols Nerd Font 12" \
		-width "15" \
		-lines 4 -line-margin 3 -line-padding 10 -scrollbar-width "0"
)

if [ "$selected_option" == "$lock" ]; then
	xset dpms force off
elif [ "$selected_option" == "$logout" ]; then
	session=$(loginctl session-status | head -n 1 | awk '{print $1}')
	loginctl terminate-session $session -o cmd -f shutdown
elif [ "$selected_option" == "$shutdown" ]; then
	systemctl poweroff
elif [ "$selected_option" == "$reboot" ]; then
	systemctl reboot
elif [ "$selected_option" == "$sleep" ]; then
	systemctl suspend
else
	echo "No match"
fi
