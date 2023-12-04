{pkgs, ...}: {

  home.packages = with pkgs; [

    # better to manage it via development shell

    # shell
    nodePackages.bash-language-server

    # basic stack
    # nodePackages.vscode-langservers-extracted
    hugo monolith

    ### python
    # python311Packages.python-lsp-server
    # nodePackages.pyright
    (python311.withPackages(ps: with ps; [
      python-lsp-server
      biopython
      python-lsp-black
      python-lsp-ruff
    ]))

    # core comp
    gcc gnumake tree-sitter
    parallel

    # science
    # blast
    julia

    # R
    (rWrapper.override {
      packages = with rPackages; [
        ggplot2
        # dplyr
        # tidyverse # set of 9 packages
        languageserver
        lintr
        # stringr
        BiocManager
        # httr
        # ggvis
        # shiny
        # rio
        # rmarkdown
      ]; })

    # nix
    # nixd # or nil

    # alltex
    # texlive.combined.scheme-full
    texliveSmall
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
