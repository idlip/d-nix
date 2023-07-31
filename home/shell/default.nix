{config, ...}: let
  d = config.xdg.dataHome;
  c = config.xdg.configHome;
  cache = config.xdg.cacheHome;
in {
  imports = [
    ./cli.nix
    ./git.nix
    ./starship.nix
    ./nix.nix
    ./code.nix
    ./neovim.nix
    ./terminal.nix
    ./zsh.nix
  ];

  # add environment variables
  home.sessionVariables = {
    # clean up ~
    LESSHISTFILE = cache + "/less/history";
    LESSKEY = c + "/less/lesskey";
    WINEPREFIX = d + "/wine";
    XAUTHORITY = "$XDG_RUNTIME_DIR/Xauthority";

    # enable scrolling in git diff
    DELTA_PAGER = "less -R";

    EDITOR = "emacsclient -nw -a 'nvim'";
    MANPAGER = "sh -c 'col -bx | bat -l man -p'";
    DIRENV_LOG_FORMAT = "";
  };
}
