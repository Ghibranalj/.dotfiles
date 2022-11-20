#!/usr/bin/env bash


# Required packages
# 1. nm-applet
# 2. inputlug

export DISPLAY=":0"
$HOME/.bin/screen.sh &


function killstart(){
    killall -KILL "$1"
    $@
}

unclutter --timeout 60 --jitter 3 &
killstart nm-applet &
killstart inputplug -0 --command $HOME/.bin/inputplug.sh &

$HOME/.bin/screensaverd &
$HOME/.onStartup.sh &
sleep 1
feh "$HOME/.local/share/dwm/background.png" --bg-scale &
