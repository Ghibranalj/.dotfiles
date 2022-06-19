#!/bin/bash

echo "installing dotfiles"

gsettings set org.gnome.settings-daemon.plugins.media-keys volume-step 2
dconf load / <data/dconf-configs

echo '--------------------------------'
echo '====== Installing Themes ======='
echo '--------------------------------'

git clone https://github.com/vinceliuice/Orchis-theme.git vendor/Orchis
chmod +x vendor/Orchis/install.sh
vendor/Orchis/install.sh -t orange

if command -v gnome-tweaks &>/dev/null; then
    echo 'Select your theme'
    gnome-tweaks &>/dev/null
fi

if command -v cinnamon-settings &>/dev/null; then
    echo 'Select your theme'
    cinnamon-settings &>/dev/null
fi
