{pkgs, ...}: {
  home.packages = with pkgs; [
    # archives
    zip
    unzip

    fzf
    # wonderful spells
    aspell aspellDicts.en-science aspellDicts.en hunspell hunspellDicts.en-us

    ffmpeg sdcv nq
    # utils
    file
    du-dust
    fd
    wget

  ];

  programs = {
    nix-index.enable = true;
    exa = {
      enable = true;
      extraOptions = ["--group-directories-first" "--header"];
      icons = true;
    };

    ripgrep = {
      enable = true;
      arguments = [
        "--max-columns-preview"
        "--colors=line:style:bold"
      ];
    };

    btop = {
      enable = true;
      settings = {
        color_theme = "Default";
        theme_background = false;
        vim_keys = true;
        shown_boxes = "proc cpu";
        rounded_corners = true ;
        graph_symbol = "block";
        proc_sorting = "memory";
        proc_reversed = false;
        proc_gradient = true;
      };
    };

    bat = {
      enable = true;
      extraPackages = with pkgs.bat-extras; [ batdiff batman batgrep batwatch ];
      config = {
        pager = "less -FR";
        theme = "TwoDark";
      };
    };

    ssh.enable = true;

    skim = {
      enable = true;
      enableZshIntegration = true;
      defaultCommand = "rg --files --hidden";
      changeDirWidgetOptions = [
        "--preview 'exa --icons --git --color always -T -L 3 {} | head -200'"
        "--exact"
      ];
    };
  };
}
