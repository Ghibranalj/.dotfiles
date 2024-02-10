#!/usr/bin/env bash

# installing emacs
yay -S emacs-native-comp
\rm -rf ~/.config/emacs || echo Installing From Scratch
git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.config/emacs
~/.config/emacs/bin/doom install
