{
  ...
}:
# glue all configs together
{

  config.home.stateVersion = "23.05";

  imports = [
    ./shell
    ./programs
    ./wayland
  ];
}
