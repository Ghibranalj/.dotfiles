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
systemctl restart --user blugon
start discord
sleep 10
xdotool search "Virtual Machine Manager" windowclose
xdotool search 'virt-manager' windowclose
