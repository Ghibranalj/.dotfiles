#!/bin/bash


sudo pacman -S --needed base-devel
git clone https://aur.archlinux.org/yay.git vendor/yay
cd vendor/yay
makepkg -si
cd ../..

yay -S google-chrome spotify discord whatsapp-for-linux visual-studio-code-bin neovim
ln usr/local/bin/whatsapp-for-linux /usr/local/bin/whatsapp