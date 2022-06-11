#!/usr/bin/env bash

# installing emacs
 yay -S emacs

git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.emacs.d
~/.emacs.d/bin/doom install
