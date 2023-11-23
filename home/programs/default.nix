{
  pkgs,
  config,
  ...
}: {

  imports = [
    ./xdg.nix
    ./qt.nix
    ./browser.nix
    ./gtk.nix
    ./aria2.nix
    ./sioyek.nix
    ./emacs.nix
    ./dunst.nix
    # ./games.nix
    ./packages.nix
    ./media.nix
    ./rofi.nix
  ];

  programs = {
    man = {
      enable = true;
      generateCaches = true; # will take little time
    };

    gpg = {
      enable = true;
      homedir = "${config.xdg.dataHome}/gnupg";
    };
  };

  services = {
    gpg-agent = {
      enable = true;
      enableSshSupport = true;
      enableZshIntegration = true;
      pinentryFlavor = "gnome3";
    };

  };
}
