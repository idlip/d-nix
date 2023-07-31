{
  pkgs,
  config,
  ...
}: {

  imports = [
    ./xdg.nix
    # ./qt.nix
    ./gtk.nix
    ./aria2.nix
    ./sioyek.nix
    ./emacs.nix
    ./dunst.nix
    # ./games.nix
    ./packages.nix
    ./media.nix
  ];

  programs = {
    chromium = {
      enable = true;
      package = pkgs.brave;
      commandLineArgs = ["--enable-features=TouchpadOverscrollHistoryNavigation"];
      extensions = [
        {id = "cjpalhdlnbpafiamejdnhcphjbkeiagm";} # ublock origin
        {id = "bkkmolkhemgaeaeggcmfbghljjjoofoh";} # catppuccin
        {id = "dbepggeogbaibhgnhhndojpepiihcmeb";} # vimium
        {id = "eimadpbcbfnmbkopoojfekhnkhdbieeh";} # dark-reader
        {id = "lljedihjnnjjefafchaljkhbpfhfkdic";} # jiffy reader
        {id = "mnjggcdmjocbbbhaepdhchncahnbgone";} # sponsorblock
      ];
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
