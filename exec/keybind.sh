#!/bin/bash

# from this article :
# https://solaaremupelumi.medium.com/persistent-keyboard-mapping-on-ubuntu-using-xmodmap-cd01ad828fcd


read -p "Reset keys? [y/N]" -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]
then
    sleep 1
    xmodmap data/defaultKeymapping
    echo "resetting done"
    sleep 5
fi

xmodmap ./exec/Xmodmap
xkbcomp $DISPLAY $PWD/my_layout.xkbd
./exec/toLayout.py > us.tmp
sudo cp us.tmp /usr/share/X11/xkb/symbols/us

dconf load / < data/dconf-keys