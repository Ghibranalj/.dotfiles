#!/usr/bin/env sh

export YOFF=25px
export INPUT=false
export WIDTH=500px
export HEIGHT=50%
rofi -location 3 -show b -modi "b:$HOME/.config/rofi-scripts/vm"
