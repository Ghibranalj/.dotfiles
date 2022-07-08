#!/usr/bin/env bash

# installing emacs
yay -S emacs

git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.emacs.d
git clone --depth=1 -b master https://github.com/emacs-eaf/emacs-application-framework.git ~/.emacs.d/site-lisp/emacs-application-framework/

~/.emacs.d/bin/doom install
