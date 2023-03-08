#!/usr/bin/env bash

# Required packages
# 1. nm-applet
# 2. inputlug

function killstart(){
    killall -KILL "$1"
    $@
}

export DISPLAY=":0"

[[ `cat /etc/hostname` == *"CreeprTUF"* ]]
    $HOME/.bin/screen.sh &

[[ `cat /etc/hostname` == *"CreeprDell"* ]] &&
    $HOME/.bin/1080p.sh

unclutter --timeout 60 --jitter 3 &
killstart nm-applet &
killstart inputplug -0 --command $HOME/.bin/inputplug.sh &

$HOME/.bin/screensaverd &
$HOME/.onStartup.sh &
sleep 1
feh "$HOME/.local/share/dwm/background.png" --bg-scale &
