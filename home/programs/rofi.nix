{
  pkgs,
  config,
  ...
}: {

  # programs.rofi = {
  #   enable = true;
  #   package = pkgs.rofi-wayland;
  #   font = "Code D OnePiece 16";
  #   location = "center";
  # };

  home.packages = with pkgs; [
    rofi-wayland
  ];


  xdg.configFile."rofi/config.rasi".source = config.lib.file.mkOutOfStoreSymlink "/home/idlip/d-git/d-nix/home/programs/d-rofi/config.rasi";

  xdg.configFile."rofi/smenu.rasi".source = config.lib.file.mkOutOfStoreSymlink "/home/idlip/d-git/d-nix/home/programs/d-rofi/smenu.rasi";

  xdg.configFile."rofi/list.rasi".source = config.lib.file.mkOutOfStoreSymlink "/home/idlip/d-git/d-nix/home/programs/d-rofi/list.rasi";

  xdg.configFile."rofi/grid.rasi".source = config.lib.file.mkOutOfStoreSymlink "/home/idlip/d-git/d-nix/home/programs/d-rofi/grid.rasi";

}
