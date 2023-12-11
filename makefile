.PHONY: all exec conf optimus udev keybind emacs stow vim dwm systemd systemdroot fonts
.ONESHELL:

all: stow rootstow exec conf emacs vim systemd

exec:
	./exec/app-arch.sh
	./exec/conf.sh
	./exec/keybind.sh

conf:
	sudo cp --force configs/bluetooth/* /etc/bluetooth/
	sudo cp --force configs/faillock/* /etc/security/

stow:
	stow --adopt home
	stow --no-folding --adopt systemd

rootstow:
	sudo stow --adopt root -t /

optimus:
	./exec/gpu-nightmare.sh

udev :
	sudo rm -f /etc/udev/rules.d/*
	sudo cp udev/*	/etc/udev/rules.d/
	sudo chown root:root /etc/udev/rules.d/*
	sudo udevadm control --reload-rules
	sudo udevadm trigger

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

WIN10_DESKTOP_PATH = "/usr/share/xsessions/win10.desktop"
win10login:
	echo [Desktop Entry] > "$(WIN10_DESKTOP_PATH)"
	echo Name=Windows 10 >> "$(WIN10_DESKTOP_PATH)"
	echo Comment=Start Windows 10 VM Passthrough >> "$(WIN10_DESKTOP_PATH)"
	echo Exec=C:\path\to\start-win10.bat >> "$(WIN10_DESKTOP_PATH)"
	echo Icon=windows >> "$(WIN10_DESKTOP_PATH)"
	echo Type=XSession >> "$(WIN10_DESKTOP_PATH)"

dwm:
	./exec/dwm.sh

fonts:
	./exec/install_fonts.sh
