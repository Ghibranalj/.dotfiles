.PHONY: all exec conf optimus udev keybind emacs stow vim dwm systemd systemdroot

all: stow rootstow exec conf emacs vim systemd

exec:
	./exec/app-arch.sh
	./exec/conf.sh
	./exec/keybind.sh

conf:
	sudo cp --force configs/bluetooth/* /etc/bluetooth/

stow:
	stow --adopt home
	stow --no-folding --adopt systemd

rootstow:
	sudo stow --adopt root -t /

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
	sudo cp -f systemd-root/etc/systemd/system/* /etc/systemd/system/
	sudo systemctl daemon-reload
	sudo systemctl enable --now $(ROOT_SERVICES)

dwm:
	./exec/dwm.sh
