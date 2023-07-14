{ inputs, pkgs, config, lib, self, ...}:
{
  config.home.stateVersion = "23.05";
  config.home.extraOutputsToInstall = ["doc" "devdoc"];
  imports = [
    ./homeman.nix
    # inputs.nur.nixosModules.nur
  ];
}
