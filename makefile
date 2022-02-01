
TARGET: exec conf

exec:
	./exec/app-arch.sh
	./exec/conf.sh
	./exec/keybind.sh
conf:
	stow --dir=./home --target=~
	mkdir -p ~/.config/nvim
	stow --dir=./nvim --target=~/.config/nvim
	curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
	
	echo ++ SETTING UP BLUETOOTH ++
	sudo rm /etc/bluetooth/main.conf
	sudo cp configs/bluetooth/main.conf /etc/bluetooth/main.conf

optimus: 
	./exec/gpu-nightmare.sh