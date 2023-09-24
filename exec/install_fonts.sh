#!/usr/bin/env sh

clear
cat <<EOF
Installing Fonts
==================
The package's size is around 16GB
This may take a while

EOF

yay -Syy noto-fonts-emoji nerd-fonts-git --sudoloop
