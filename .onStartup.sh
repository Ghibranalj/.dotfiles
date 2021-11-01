#!/bin/bash

discord &disown
spotify &disown

sleep 20
xkbcomp /home/gibi/.xkbmap :1 &> /home/gibi/.keybind.log
gsettings set org.gnome.settings-daemon.plugins.keyboard active false

