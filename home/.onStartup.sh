#!/bin/bash
function start() {
	$@ &
	disown
}

start activate-linux
start spotify
start discord
