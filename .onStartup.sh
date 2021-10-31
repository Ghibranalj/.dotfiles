#!/bin/bash

discord &disown
spotify &disown

sleep 10
xkbcomp /home/gibi/.xkbmap :1 &> /home/gibi/.keybind.log

