#!/bin/bash

if [ "$EUID" -ne 0 ]
  then echo "Please run as root"
  exit
fi
echo 'installing aplications' 

apt update
apt full-upgrade

snap install code --classic
snap install discord spotify
apt install gnone-tweaks curl blueman 

## installing chrome
wget -q -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | apt-key add -
echo 'deb [arch=amd64] http://dl.google.com/linux/chrome/deb/ stable main' | tee /etc/apt/sources.list.d/google-chrome.list
apt update 
apt install google-chrome-stable
ln /usr/bin/google-chrome /usr/bin/chrome
#

## installing node 17
curl -s https://deb.nodesource.com/gpgkey/nodesource.gpg.key | apt-key add -
sh -c "echo deb https://deb.nodesource.com/node_17.x impish main \
> /etc/apt/sources.list.d/nodesource.list"
apt update
apt install nodejs
#


