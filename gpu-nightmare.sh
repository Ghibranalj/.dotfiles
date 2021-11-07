#!/bin/bash

yay -S gdm-prime

sudo rm -rf etc/gdm/custom.conf
sudo cp config/gdm/custom.conf etc/gdm/custom.conf

yay -s optimus-manager optimus-manager-qt
sudo cp config/optimus-manager/optimus-manager.conf /etc/optimus-manager/optimus-manager.conf

reboot