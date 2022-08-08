#!/bin/env bash

if [[ $(hostname) == "CreeprTUF" ]]; then
    xrandr --output eDP-1 --primary --mode 1920x1080 --pos 0x0 --rotate normal --output HDMI-1-0 --mode 2560x1080 --pos 1920x0 --rotate normal
fi
