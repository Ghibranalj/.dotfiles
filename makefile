.PHONY: all exec conf optimus udev keybind emacs gesture stow

all: exec conf emacs

exec:
	./exec/app-arch.sh
	./exec/conf.sh
	./exec/keybind.sh
conf:
	curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim

	echo ++ SETTING UP BLUETOOTH ++
	sudo rm /etc/bluetooth/main.conf
	sudo cp configs/bluetooth/main.conf /etc/bluetooth/main.conf

stow:
	stow --adopt home
	stow --adopt nvim
	stow --adopt alacritty

optimus: 
	./exec/gpu-nightmare.sh

udev :
	sudo cp udev/*	/etc/udev/rules.d/

keybind:
	./exec/keybind.sh

emacs:
	stow --adopt doom
	./exec/emacs.sh

gesture:
	./gesture/exec.sh

bin:
	stow --adopt bin
