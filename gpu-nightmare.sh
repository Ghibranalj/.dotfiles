#!/bin/bash

yay -S gdm-prime

sudo rm -rf etc/gdm/custom.conf
sudo cp configs/gdm/custom.conf /etc/gdm/custom.conf

yay -S optimus-manager optimus-manager-qt
sudo cp configs/optimus-manager/optimus-manager.conf /etc/optimus-manager/optimus-manager.conf


echo "rebooting in 10 seconds"
sleep 10

reboot
