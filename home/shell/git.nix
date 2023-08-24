{
  pkgs,
  # default,
  ...
}: {
  home.packages = [pkgs.gh];

  programs.git = {
    enable = true;
    package = pkgs.gitFull;

    delta = {
      enable = true;
      # TODOTHIS
      # options.map-styles = "bold purple => syntax ${default.xcolors.mauve}, bold cyan => syntax ${default.xcolors.blue}";
    };

    extraConfig = {
      diff.colorMoved = "default";
      merge.conflictstyle = "diff3";
    };

    aliases = {
      a = "add";
      b = "branch";
      c = "commit";
      ca = "commit --amend";
      cm = "commit -m";
      cho = "checkout";
      d = "diff";
      ds = "diff --staged";
      p = "push";
      pf = "push --force-with-lease";
      pl = "pull";
      l = "log";
      r = "rebase";
      s = "status --short";
      ss = "status";
      forgor = "commit --amend --no-edit";
      graph = "log --all --decorate --graph --oneline";
      logrep = "log --decorate --oneline --grep=";
      oops = "checkout --";
    };

    ignores = ["*~" "*.swp" "*result*" ".direnv" "node_modules"];

    # signing = {
    #   key = "";
    #   signByDefault = true;
    # };

    userEmail = "igoldlip@gmail.com";
    userName = "Dilip";
    # editor = "";
  };
}
