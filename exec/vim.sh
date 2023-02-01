#!/usr/bin/env bash

yay -S neovim-nightly-bin rust
LV_BRANCH=rolling bash <(curl -s https://raw.githubusercontent.com/lunarvim/lunarvim/rolling/utils/installer/install.sh)
