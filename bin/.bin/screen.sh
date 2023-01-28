#!/bin/sh
prime-offload

MODE=$(optimus-manager --print-mode | awk -F': ' '{print $2}')

if [[ "$MODE" == "integrated" ]]; then
  echo hello world
else
xrandr --output HDMI-0 --mode 2560x1080 --pos 1920x0 --rotate normal --output eDP-1-1 --primary --mode 1920x1080 --pos 0x0 --rotate normal
fi
