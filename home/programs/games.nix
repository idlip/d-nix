{
  pkgs,
  ...
}:
# games
{
  home.packages = with pkgs; [
    openttd
    zeroadPackages.zeroad-unwrapped
    superTux
    superTuxkart
  ];
}
