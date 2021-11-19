#!/bin/bash


discord &disown
spotify &disown
whatsapp &disown
albert	&disown

xbindkeys
sleep 20
xkbcomp /home/gibi/.xkbmap :1 &> /home/gibi/.keybind.log
inputplug -d -c ~/.on-new-kbd.sh &disown
# gsettings set org.gnome.settings-daemon.plugins.keyboard active false
