.PHONY: all exec conf optimus udev keybind emacs stow vim dwm systemd systemdroot

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

SERVICES = $(shell ls systemd/.config/systemd/user | grep \.service)
systemd:
	stow --no-folding --adopt systemd
	systemctl --user daemon-reload
	systemctl --user enable $(SERVICES)

ROOT_SERVICES = $(shell ls systemd-root/etc/systemd/system | grep \.service)
systemdroot:
	sudo stow --no-folding --adopt systemd-root -t /
	sudo systemctl daemon-reload
	sudo systemctl enable --now $(ROOT_SERVICES)

dwm:
	./exec/dwm.sh
