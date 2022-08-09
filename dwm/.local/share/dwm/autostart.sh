#!/usr/bin/env bash
source ~/.bashrc

~/.bin/screen.sh &
~/.onStartup.sh &

function disable-accel() {
    while true; do
        for id in $(xinput list | grep "pointer" | cut -d '=' -f 2 | cut -f 1); do
            xinput --set-prop "$id" 'libinput Accel Profile Enabled' 0, 1
        done
        sleep 1
    done
}
disable-accel &
blueman-applet &
nm-applet &
pasystray &
~/.bin/dwm-bar.sh &
killall -KILL emacs &

sleep 1
feh /usr/share/backgrounds/gnome/blobs-d.svg --bg-scale &
