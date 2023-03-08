#!/bin/sh
prime-offload

if [[ `cat /etc/hostname` == *"CreeprTUF"* ]]; then
  xrandr --output HDMI-0 --mode 2560x1080 --pos 1920x0 --rotate normal --output eDP-1-1 --primary --mode 1920x1080 --pos 0x0 --rotate normal
  exit 0
fi

