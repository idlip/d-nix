#!/usr/bin/env bash
# Read and edit these once accordingly

echo "Installing System Flake"
sudo cp -f /etc/nixos/hardware-configuration.nix /home/i/.DLIP/SETUP/gdk/hardware-configuration.nix

# Edit it your file path

#cd /home/i/.DLIP/SETUP
sudo nixos-rebuild switch --flake .#gdk

echo "Copying Installation Checklist"
cp ./gdk/manual-init-steps.org ~/manual-init-steps.org

echo "Restarting System"
reboot
