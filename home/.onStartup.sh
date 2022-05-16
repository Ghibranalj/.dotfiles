#!/bin/bash

function start(){
	$1 &disown
}

start albert
start discord
start spotify
start noisetorch
# start whatsapp

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
