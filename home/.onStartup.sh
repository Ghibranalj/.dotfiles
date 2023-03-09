#!/bin/bash
function start() {
	$@ &
	disown
}

start activate-linux
start spotify
start discord
start syndaemon -i 0.5 -t -k -R
