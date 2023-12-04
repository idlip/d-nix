{
  pkgs,
  ...
}:
# games
{
  home.packages = with pkgs; [
    openttd
    zeroad
    superTux
    # superTuxkart
  ];
}
