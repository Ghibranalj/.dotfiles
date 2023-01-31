#!/bin/bash
function start() {
	$@ &
	disown
}

start spotify
start discord
