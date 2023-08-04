{
  inputs,
  ...
}:
# glue all configs together
{

  # NEVER EVER TOUCH IT ⚠️
  config.home.stateVersion = "23.05"; # OR CRY WITH BROKE SYS

  imports = [
    inputs.nix-index-db.hmModules.nix-index
    ./shell
    ./programs
    ./wayland
  ];
}
