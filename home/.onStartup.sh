#!/bin/bash
function start() {
	$@ &
	disown
}

start spotify
start syndaemon -i 0.5 -t -k -R
sleep 2
killall emacs
systemctl restart --user blugon
start discord
