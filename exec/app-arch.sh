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
yay -Sy neovim advcpmv exa xclip xbindkeys blueman bluetooth-autoconnect \
	most noisetorch alacritty tabbed rofi rofi-emoji rofi-calc rmtrash

# other apps
yay -Sy google-chrome spotify discord whatsapp-for-linux

# timehshift
yay -Sy timehshift

# for dwm
yay -Sy tabbed-git feh dwm

# for dwm status bar
yay -Sy ponymix dunst fd gdbmtool gdbm pactl xprintidle xsecurelock pamixer \
	acpi pamixer adwaita-icon-theme-41 brightnessctl bc sox

# for utility
yay -Sy udiksie spotifyd

sudo systemctl enable bluetooth-autoconnect
systemctl --user enable pulseaudio-bluetooth-autoconnect

function link() {
	sudo ln $(which $1) $(dirname $(which $1))/$2
}

link whatsapp-for-linux whatsapp
link google-chrome-stable chrome

# fonts
# yay -Sy nerd-fonts-complete

echo "Done!!"
