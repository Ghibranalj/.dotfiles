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
	stow --adopt home
	stow --adopt alacritty
	stow --adopt rofi

optimus: 
	./exec/gpu-nightmare.sh

udev :
	sudo cp udev/*	/etc/udev/rules.d/

keybind:
	./exec/keybind.sh

emacs:
	stow --adopt doom
	./exec/doom-emacs.sh

vim:
	./exec/vim.sh
	rm ~/.config/lvim -rv
	stow --adopt lvim

gesture:
	./gesture/exec.sh

bin:
	stow --adopt bin

dconf-dump:
	dconf dump / > data/dconf-configs

systemd:
	ln -Pf systemd/user/*.service ~/.config/systemd/user/
	systemctl --user daemon-reload
	systemctl enable --user --now emacs emacs-term
	systemctl enable --user --now xbindkeys

spotifyd:
	stow --adopt spotifyd
	yay -S spotifyd
	systemctl enable --user --now spotifyd

spotify:
	./exec/spicetify.sh

dwm: xorg
	stow dwm

xorg:
	sudo cp xorg/* /etc/X11/xorg.conf.d/
