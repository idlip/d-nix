{pkgs, ...}: {

  home.packages = with pkgs; [

    nodePackages.bash-language-server
    # nodePackages.vscode-langservers-extracted
    # python311Packages.python-lsp-server
    nodePackages.pyright
    python3 gcc gnumake
    R rPackages.languageserver rPackages.lintr
    nixd # or nil
    tree-sitter
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
