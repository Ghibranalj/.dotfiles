#!/bin/env bash

function link-usb() {
	if [[ $(ls /run/media/$USER/) == "" ]]; then
		echo "No USB devices found"
		unlink "$HOME"/USB/*
		rm -d "$HOME"/USB
		return
	fi
	if [[ ! -d $HOME/USB ]]; then
		echo "Creating USB directory"
		mkdir "$HOME/USB"
	fi

	for d in /run/media/"$USER"/*; do
		echo "Linking $d"
		ln -s "$d" "$HOME/USB/"
	done
}

while true; do
	link-usb
	sleep 10
done &
