#!/bin/bash

if [ "$EUID" -ne 0 ]
  then echo "Please run as root"
  exit
fi
echo 'installing aplications' 

apt update
apt full-upgrade

#installing chrome

wget -q -O - https://dl-ssl.google.com/linux/linux_signing_key.pub | apt-key add -
echo 'deb [arch=amd64] http://dl.google.com/linux/chrome/deb/ stable main' | tee /etc/apt/sources.list.d/google-chrome.list
apt update 
apt install google-chrome-stable
ln /usr/bin/google-chrome /usr/bin/chrome
#
snap install code --classic
snap install discord spotify
apt install gnone-tweaks


