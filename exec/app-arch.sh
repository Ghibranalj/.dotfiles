#!/bin/bash

#GNU stow
sudo pacman -S stow

# bluetooth
sudo pacman -S --needed bluez bluez-utils
sudo systemctl enable --now bluetooth

if ! command -v yay &>/dev/null; then
	sudo pacman -S --needed base-devel
	git clone https://aur.archlinux.org/yay.git vendor/yay
	cd vendor/yay
	makepkg -si
	cd ../..
fi

# needed by dotfiles
yay -Sy neovim advcpmv exa xclip xbindkeys blueman bluetooth-autoconnect most noisetorch alacritty tabbed

# other apps
yay -Sy google-chrome spotify discord whatsapp-for-linux albert

# timehshift
yay -Sy timehshift

albert &>/dev/null &
sudo systemctl enable bluetooth-autoconnect
systemctl --user enable pulseaudio-bluetooth-autoconnect

function link() {
	sudo ln $(which $1) $(dirname $(which $1))/$2
}

link whatsapp-for-linux whatsapp
link google-chrome-stable chrome

# fonts
yay -S nerd-fonts-complete
