{
  config,
  pkgs,
  ...
}: {
  imports = [
    ./core.nix
    ./bootloader.nix
    ./desktop.nix
    ./network.nix
    ./nix.nix
    ./security.nix
    ./bluetooth.nix
  ];
}
