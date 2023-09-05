#!/bin/bash
function start() {
	$@ &
	disown
}

start spotify
start syndaemon -i 0.5 -t -k -R
start virt-manager
sleep 2
killall emacs
start discord
xdotool search "Virtual Machine Manager" windowclose
xdotool search 'virt-manager' windowclose

sleep 10
xdotool search "Virtual Machine Manager" windowclose
xdotool search 'virt-manager' windowclose
