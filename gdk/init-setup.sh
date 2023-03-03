#! /bin/bash

echo "Installing System Flake"
sudo cp -f /etc/nixos/hardware-configuration.nix ~/.SETUP/gdk/hardware-configuration.nix
cd ~/.SETUP
sudo nixos-rebuild switch --flake .#gdk

echo "Creating SSH Key"
ssh-keygen -f /home/que/.ssh/id_rsa -q -N ""
cp ~/.ssh/id_rsa.pub ~/ssh-key

echo "Copying Installation Checklist"
cp ./gdk/manual-init-steps.org ~/manual-init-steps.org

echo "Restarting System"
reboot
