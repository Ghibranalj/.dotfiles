#!/bin/bash

function start() {
	$@ &
	disown
}

start xbindkeys
start spotify
start noisetorch -s alsa_input.usb-Generalplus_Usb_Audio_Device-00.mono-fallback -i
# emacsclient --create-frame --no-wait
sleep 3
start discord
function link-usb() {
	if [[ $(ls /run/media/$USER/) == "" ]]; then
		unlink $HOME/USB/*
		rm -d $HOME/USB
		return
	fi
	if [[ ! -d $HOME/USB ]]; then
		mkdir "$HOME/USB"
	fi

	for d in /run/media/$USER/*; do
		ln -s "$d" "$HOME/USB/"
	done
}

while true; do
	link-usb
	sleep 10
done &
