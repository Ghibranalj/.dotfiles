#!/usr/bin/env sh

git clone https://github.com/ghibranalj/dwm-flexipatch.git vendor/dwm-flexipatch
cd vendor/dwm-flexipatch
sudo make install
cd ../..
