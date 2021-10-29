#!/bin/bash

echo "installing dotfiles"


function toHome() {
    ln $1 ~/$1
}

toHome .bashrc
toHome .gitconfig
toHome .xmodmap
toHome .xmodmap.sh
chmod +x ~/.xmodmap.sh
toHome .onStartup.sh
chmod +x ~/.onStartup.sh 
