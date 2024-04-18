#!/bin/sh

HOSTNAME=`cat /etc/hostname | tr -d '[:space:]'`

if [[ $HOSTNAME == "CreeprTUF" ]]; then
  prime-offload
  xrandr --output HDMI-0 --mode 2560x1080 --pos 1920x0 --rotate normal --output eDP-1-1 --primary --mode 1920x1080 --pos 0x0 --rotate normal
  exit 0
fi


if [[ $HOSTNAME == "CreeprPC" ]]; then
  xrandr --output DisplayPort-0 --off --output DisplayPort-1 --off --output DisplayPort-2 --mode 1920x1080 --rate 100 --pos 1920x0 --rotate normal --output HDMI-A-0 --mode 1920x1080 --pos 0x0 --rotate normal
  exit 0
fi

if [[ $HOSTNAME == "CreeprDell" ]]; then
  xrandr --newmode "1920x1080_60.00"  173.00  1920 2048 2248 2576  1080 1083 1088 1120 -hsync +vsync
  xrandr --addmode eDP-1 1920x1080_60.00
  xrandr -s 1920x1080_60.00
  exit 0
fi
