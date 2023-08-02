{
  inputs,
  ...
}:
# glue all configs together
{

  config.home.stateVersion = "23.05";

  imports = [
    inputs.nix-index-db.hmModules.nix-index
    ./shell
    ./programs
    ./wayland
  ];
}
