#!/bin/bash

# from this article :
# https://solaaremupelumi.medium.com/persistent-keyboard-mapping-on-ubuntu-using-xmodmap-cd01ad828fcd

xmodmap Xmodmap
xkbcomp $DISPLAY $HOME/.xkbmap
