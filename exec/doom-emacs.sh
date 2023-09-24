#!/usr/bin/env bash

# installing emacs
yay -S emacs-native-comp
\rm -rf ~/.emacs.d || echo Installing From Scratch
git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.emacs.d
~/.emacs.d/bin/doom install
