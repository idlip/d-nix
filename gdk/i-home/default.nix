{
  inputs, pkgs, config, lib, self, ...}:
{
  config.home.stateVersion = "23.05";
  config.home.extraOutputsToInstall = ["doc" "devdoc"];
  imports = [
    ./homeman.nix
    # inputs.hyprland.homeManagerModules.default
    # inputs.nur.nixosModules.nur
  ];
}
