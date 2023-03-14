# FIXME Chaneg hostname and directory under :tangle ...
{
  inputs, pkgs, config, lib, self, ...}:

{
  config.home.stateVersion = "22.11";
  config.home.extraOutputsToInstall = ["doc" "devdoc"];
  imports = [
    ./homeman.nix
    inputs.hyprland.homeManagerModules.default
    inputs.nur.nixosModules.nur
  ];
}
