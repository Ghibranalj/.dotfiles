#!/bin/bash
function start() {
	if pgrep -f $1; then
		echo "$1 is already running"
		return
	fi
	# if the program doesnt exist, just return
	if ! command -v $1 &> /dev/null; then
		echo "$1 does not exist"
		return
	fi
	$@ &
	disown
}

start spotify
start syndaemon -i 0.5 -t -k -R
start whatsapp-for-linux
sleep 2
killall emacs
systemctl restart --user blugon
start discord
