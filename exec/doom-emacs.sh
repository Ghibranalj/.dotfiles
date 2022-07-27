#!/usr/bin/env bash

# installing emacs
# yay -S emacs-git

git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.emacs.d
git clone --depth=1 -b master https://github.com/emacs-eaf/emacs-application-framework.git ~/.emacs.d/site-lisp/emacs-application-framework/

cd ~/.emacs.d/site-lisp/emacs-application-framework/
chmod +x install-eaf.py
./install-eaf.py
cd -

~/.emacs.d/bin/doom install
