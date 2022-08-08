#!/usr/bin/env bash

if ! command -v spotify &>/dev/null; then
    yay -Syy spotify
fi

if ! command -v spicetify &>/dev/null; then
    yay -Syy spicetify-cli
fi

git clone https://github.com/Ghibranalj/Spicetify-Orchis-Colours-v2.git vendor/spotify-rice
cp -rf vendor/spotify-rice/DarkOrange ~/.config/spicetify/Themes

git clone https://github.com/spicetify/spicetify-themes.git vendor/spicetify-themes
cp -rf vendor/spicetify-themes/* ~/.config/spicetify/Themes

sudo chmod a+wr /opt/spotify
sudo chmod a+wr /opt/spotify/Apps -R

spicetify backup
spicetify config current_theme DarkOrange
# spicetify config current_theme Ziro
# spicetify config color_scheme gray-dark
spicetify apply
