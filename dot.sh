#!/bin/bash

echo "installing dotfiles"


function toHome() {
    ln $1 ~/$1
}
chsh -s /bin/bash
rm ~/.bashrc
toHome .bashrc
toHome .gitconfig

toHome .onStartup.sh
chmod +x ~/.onStartup.sh

chmod +x keybind.sh
./keybind.sh

ln custom-startup.desktop ~/.config/autostart/custom-startup.desktop

# chmod +x gnome-keybindings.pl
# ./gnome-keybindings.pl -i ./data/keys.csv


echo '--------------------------------'
echo '====== Installing Themes ======='
echo '--------------------------------'

git clone https://github.com/vinceliuice/Orchis-theme.git vendor/Orchis
chmod +x vendor/Orchis/install.sh
./vendor/Orchis/install.sh -t orange

echo 'select your theme'
gnome-tweaks &> /dev/null

