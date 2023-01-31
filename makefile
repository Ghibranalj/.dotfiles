.PHONY: all exec conf optimus udev keybind emacs gesture stow sync dconf-dump vim systemd spotifyd spotify dwm xorg

all: stow exec conf emacs vim systemd

exec:
	./exec/app-arch.sh
	./exec/conf.sh
	./exec/keybind.sh

conf:
	sudo rm /etc/bluetooth/main.conf
	sudo cp configs/bluetooth/main.conf /etc/bluetooth/main.conf

stow:
	rm -r ~/.bashrc ~/.gitconfig
	stow --adopt home
	stow --adopt alacritty
	stow --adopt rofi
	stow --adopt dunst
	stow --adopt spotifyd
	stow --adopt dwm
	stow --adopt bin
	stow --adopt lvim
	stow --adopt doom
	stow --no-folding systemd

optimus: 
	./exec/gpu-nightmare.sh

udev :
	sudo cp udev/*	/etc/udev/rules.d/

keybind:
	./exec/keybind.sh

emacs:
	./exec/doom-emacs.sh

vim:
	./exec/vim.sh

SERVICES = $(shell ls systemd/.config/systemd/user | sed 's/\.service//g')
systemd:
	stow --no-folding systemd
	systemctl --user daemon-reload
	systemctl --user enable $(SERVICES)

dwm:
	./exec/dwm.sh
