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

chmod +x keybind.sh
./keybind.sh

chmod +x gnome-keybindings.pl
./gnome-keybindings.pl -i ./data/keys.csv


echo '--------------------------------'
echo '====== Installing Themes ======='
echo '--------------------------------'

git clone https://github.com/vinceliuice/Orchis-theme.git vendor/Orchis
chmod +x vendor/Orchis/install.sh
./vendor/Orchis/install.sh

echo 'select your theme'
gnome-tweaks &> /dev/null

bash -c "$(wget -qO- https://git.io/vQgMr)"
