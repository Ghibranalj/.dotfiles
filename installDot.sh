#!/bin/bash

echo "installing dotfiles"


function toHome() {
    ln $1 ~/$1
}

toHome .bashrc
toHome .gitconfig
# toHome .xmodmap
# toHome .xmodmap.sh
chmod +x ~/.xmodmap.sh
toHome .onStartup.sh
chmod +x ~/.onStartup.sh

# put sudo commands at the buttom
echo "Needs sudo to install Xmodmap preset"
sudo ln xkeyboard /usr/lib/systemd/system-sleep/xkeyboard
sudo chmod 755 /usr/lib/systemd/system-sleep/xkeyboard
sudo ln .xmodmap /usr/lib/systemd/system-sleep/xmodmap

sudo ln xkeyboard /usr/lib/systemd/system-shutdown/xkeyboard
sudo chmod 755 /usr/lib/systemd/system-shutdown/xkeyboard

