#!/bin/bash

# bluetooth

sudo pacman -S --needed bluez bluez-utils pulseaudio-bluetooth
sudo systemctl enable --now bluetooth


if ! command -v yay &> /dev/null
then
	sudo pacman -S --needed base-devel
	git clone https://aur.archlinux.org/yay.git vendor/yay
	cd vendor/yay
	makepkg -si
	cd ../..
fi

yay -Syu google-chrome spotify discord whatsapp-for-linux visual-studio-code-bin neovim inputplug albert advcp exa xclip xbindkeys blueman bluetooth-autoconnect

albert &> /dev/null
sudo systemctl enable bluetooth-autoconnect
systemctl --user enable pulseaudio-bluetooth-autoconnect
function link (){
	sudo ln $(which $1) $(dirname $(which $1))/$2
}

link whatsapp-for-linux whatsapp
link google-chrome-stable chrome
