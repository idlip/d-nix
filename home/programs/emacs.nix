{pkgs, config, ...}:
{

  programs.emacs = {
    enable = true;
    package = pkgs.emacs29-pgtk;
    extraPackages = (epkgs: (with epkgs; [
      treesit-grammars.with-all-grammars
      eat vundo undo-fu-session flycheck helpful flycheck-languagetool ox-pandoc
      no-littering rainbow-delimiters rainbow-mode
      vertico orderless consult marginalia embark org-modern corfu cape corfu-terminal
      org olivetti nerd-icons async dirvish
      embark-consult consult-eglot consult-flycheck markdown-mode nix-mode
      reddigg hnreader mingus which-key magit webpaste org-present
      shrface shr-tag-pre-highlight nov devdocs-browser
      beframe denote tempel tempel-collection
      sdcv elfeed elfeed-org link-hint powerthesaurus jinx meow
      doom-modeline hide-mode-line el-fetch ox-hugo
      ement kind-icon speed-type vc-backup aria2
      ess
    ])
    );
  };

  home.packages = with pkgs; [
    languagetool
  ];

  home.file.".config/emacs" = {
    recursive = true;
    source = config.lib.file.mkOutOfStoreSymlink "/home/idlip/d-git/d-nix/home/programs/d-emacs";
  };

}
