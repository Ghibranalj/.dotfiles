#!/bin/bash

if ! command -v yay &> /dev/null
then
	sudo pacman -S --needed base-devel
	git clone https://aur.archlinux.org/yay.git vendor/yay
	cd vendor/yay
	makepkg -si
	cd ../..
fi

yay -S google-chrome spotify discord whatsapp-for-linux visual-studio-code-bin neovim

function link (){
	sudo ln $(which $1) $(dirname $(which $1))/$2
}

link whatsapp-for-linux whatsapp
link google-chrome-stable chrome