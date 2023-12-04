{pkgs, config, ...}:
{

  # services.emacs = {
  #   enable = true;
  #   socketActivation.enable = true;
  # };

  programs.emacs = {
    enable = true;
    package = pkgs.emacs29-pgtk;
    extraPackages = (epkgs: (with epkgs; [
      treesit-grammars.with-all-grammars
      eat vundo undo-fu-session flycheck helpful flycheck-languagetool
      no-littering rainbow-delimiters rainbow-mode
      vertico orderless consult marginalia embark org-modern corfu cape corfu-terminal
      org olivetti nerd-icons nerd-icons-completion nerd-icons-dired async dirvish
      embark-consult consult-eglot consult-flycheck markdown-mode nix-mode
      reddigg hnreader howdoyou mingus magit webpaste org-present
      shrface shr-tag-pre-highlight nov devdocs-browser
      beframe denote tempel tempel-collection avy
      sdcv elfeed elfeed-org powerthesaurus jinx meow
      doom-modeline el-fetch ox-hugo envrc dashboard
      ement kind-icon speed-type vc-backup aria2
      ess org-re-reveal auctex julia-mode
    ])
    );
  };

  home.packages = with pkgs; [
    languagetool
    mupdf poppler_utils ghostscript # for doc-view?
  ];

  xdg.configFile."emacs/lisp" = {
    recursive = true;
    source = config.lib.file.mkOutOfStoreSymlink "/home/idlip/d-git/d-nix/home/programs/d-emacs/lisp/";
  };

  xdg.configFile."emacs/init.el".source = config.lib.file.mkOutOfStoreSymlink "/home/idlip/d-git/d-nix/home/programs/d-emacs/init.el";

  xdg.configFile."emacs/early-init.el".source = config.lib.file.mkOutOfStoreSymlink "/home/idlip/d-git/d-nix/home/programs/d-emacs/early-init.el";

}
