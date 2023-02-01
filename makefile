.PHONY: all exec conf optimus udev keybind emacs stow vim dwm systemd

all: stow exec conf emacs vim systemd

exec:
	./exec/app-arch.sh
	./exec/conf.sh
	./exec/keybind.sh

conf:
	sudo rm /etc/bluetooth/main.conf
	sudo cp configs/bluetooth/main.conf /etc/bluetooth/main.conf

stow:
	stow --adopt home
	stow --no-folding --adopt systemd


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

SERVICES = $(shell ls systemd/.config/systemd/user)
systemd:
	stow --no-folding --adopt systemd
	systemctl --user daemon-reload
	systemctl --user enable $(SERVICES)

dwm:
	./exec/dwm.sh
