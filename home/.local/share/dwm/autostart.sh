#!/usr/bin/env bash

# Required packages
# 1. nm-applet
# 2. inputlug

function killstart(){
    killall -KILL "$1"
    $@
}

export DISPLAY=":0"

$HOME/.bin/screen.sh &

if [ -f $HOME/.screenlayout/main.sh ]; then
    $HOME/.screenlayout/main.sh
fi

unclutter --timeout 60 --jitter 3 &
killstart nm-applet &

$HOME/.bin/screensaverd &
$HOME/.onStartup.sh &
sleep 1
feh "$HOME/.local/share/dwm/background.png" --bg-scale &
