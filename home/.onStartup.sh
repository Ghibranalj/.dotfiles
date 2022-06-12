#!/bin/bash

function start(){
	$@ &disown
}

start albert
start discord
start spotify
start noisetorch
# start whatsapp
start noisetorch -s alsa_input.usb-Generalplus_Usb_Audio_Device-00.mono-fallback -i
# start /usr/bin/emacs --daemon

function link-usb() {
	if [[ `ls /run/media/$USER/` == "" ]]
	then
		unlink $HOME/USB/*
		rm -r $HOME/USB
		return
	fi
	if [[ ! -d $HOME/USB ]]
	then
		mkdir $HOME/USB
	fi

	for d in /run/media/$USER/* 
	do 
		ln -s  $d $HOME/USB/
	done	
}
while true; do
    link-usb
    sleep 2
done &
