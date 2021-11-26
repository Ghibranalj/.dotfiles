#!/bin/bash

echo "installing dotfiles"

gsettings set org.gnome.settings-daemon.plugins.media-keys volume-step 2
dconf load / < data/dconf-configs

function toHome() {
    ln $1 ~/$1
}
chsh -s /bin/bash
rm ~/.bashrc
toHome .bashrc
toHome .gitconfig
tohome .xbindkeysrc

toHome .onStartup.sh
chmod +x ~/.onStartup.sh

toHome .xbindkeysrc

ln -s ${PWD}/nvim ~/.config/
curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim


chmod +x keybind.sh
./keybind.sh


ln custom-startup.desktop ~/.config/autostart/custom-startup.desktop

# chmod +x gnome-keybindings.pl
# ./gnome-keybindings.pl -i ./data/keys.csv

toHome .on-new-kbd.sh
# inputplug -d -c ~/.on-new-kbd.sh

echo '--------------------------------'
echo '====== Installing Themes ======='
echo '--------------------------------'

git clone https://github.com/vinceliuice/Orchis-theme.git vendor/Orchis
chmod +x vendor/Orchis/install.sh
./vendor/Orchis/install.sh -t orange 

if command -v gnome-tweaks &> /dev/null
then
    echo 'Select your theme'
    gnome-tweaks &> /dev/null
fi

if command -v cinnamon-settings &> /dev/null
then
    echo 'Select your theme'
    cinnamon-settings &> /dev/null
fi

read -p "Configure bluetooth to turn on on start? [Y/n]" -n 1 -r
if [[ $REPLY =~ ^[Nn]$ ]]
then
 # do nothing 
else
    sudo rm /etc/bluetooth/main.conf
    sudo cp configs/bluetooth/main.conf /etc/bluetooth/main.conf
fi