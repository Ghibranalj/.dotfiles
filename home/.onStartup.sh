#!/bin/bash
function start() {
	$@ &
	disown
}

# start activate-linux
start spotify
start syndaemon -i 0.5 -t -k -R

sleep 2
start discord
