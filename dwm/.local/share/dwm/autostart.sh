#!/usr/bin/env bash


# Required packages
# 2. nm-applet
# 3. pasystray
# 5. inputlug

export DISPLAY=":0"
$HOME/.bin/screen.sh &

# function disable-accel() {
#     while true; do
#         for id in $(xinput list | grep "pointer" | cut -d '=' -f 2 | cut -f 1); do
#             xinput --set-prop "$id" 'libinput Accel Profile Enabled' 0, 1
#         done
#         sleep 20
#     done
#

function killstart(){
    killall -KILL "$1"
    $@
}

# disable-accel &
killstart nm-applet &
# killstart pasystray &
killstart inputplug -0 --command $HOME/.bin/inputplug.sh &
# killall -KILL emacs &
# xscreensaver -no-splash &
$HOME/.bin/screensaverd &
$HOME/.onStartup.sh &
sleep 1
feh "$HOME/.local/share/dwm/background.png" --bg-scale &
