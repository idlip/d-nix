{pkgs, ...}: {

  home.packages = with pkgs; [

    # shell
    nodePackages.bash-language-server

    # basic stack
    # nodePackages.vscode-langservers-extracted

    ### python
    # python311Packages.python-lsp-server
    # nodePackages.pyright
    (python311.withPackages(ps: with ps; [
      python-lsp-server
      biopython
      pylint
      black
      rope
      flake8
    ]))

    # core comp
    gcc gnumake tree-sitter

    # R
    (rWrapper.override {
      packages = with rPackages; [
        ggplot2
        dplyr
        languageserver
        lintr
        dplyr
        tidyr
        stringr
        httr
        ggvis
        ggplot2
        shiny
        rio
        rmarkdown
      ]; })

    # nix
    nixd # or nil

    # alltex
    texlive.combined.scheme-full
  ];

  programs = {

    gpg.enable = true;
    man.enable = true;

    direnv = {
      enable = true;
      nix-direnv.enable = true;
      enableZshIntegration = true;
    };

    tealdeer = {
      enable = true;
      settings = {
        display = {
	        compact = false;
	        use_pager = true;
        };
        updates = {
	        auto_update = true;
        };
      };
    };

  };

}
