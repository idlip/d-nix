# WARNING : This file was generated by d-setup.org
# DO NOT MODIFY THIS FILE!
# Any changes made here will be overwritten.

{ inputs, pkgs, self, lib, config, ... }:

{

home.sessionVariables.STARSHIP_CACHE = "${config.xdg.cacheHome}/starship";
programs = {
  exa.enable = true;
  starship = {
    enable = true;
    enableZshIntegration = true;
    settings = {
      add_newline = true;
      scan_timeout = 5;
      character = {
        error_symbol = " [](#df5b61)";
        success_symbol = "[](#6791c9)";
        vicmd_symbol = "[](bold yellow)";
        format = "[   $directory$all$character  ](bold)";
      };
      git_commit = {commit_hash_length = 4;};
      line_break.disabled = false;
      lua.symbol = "[](blue) ";
      python.symbol = "[](blue) ";
      directory.read_only = " ";
      nix_shell.symbol = " ";
      hostname = {
        ssh_only = true;
        format = "[$hostname](bold blue) ";
        disabled = false;
      };
    };
  };

  fish = {
    enable = true;
    shellInit = ''
        starship init fish | source
        set -x FZF_DEFAULT_OPTS "--preview='bat {} --color=always'" \n
  set -x SKIM_DEFAULT_COMMAND "rg --files || fd || find ."
  set -g theme_nerd_fonts yes
  set -g theme_newline_cursor yes
  set fish_greeting
        '';

    shellAliases = with pkgs; {
      rebuild = "doas nix-store --verify; pushd ~dotfiles && doas nixos-rebuild switch --flake .# && notify-send \"Done\"&& bat cache --build; popd";
      cleanup = "doas nix-collect-garbage --delete-older-than 7d";
      bloat = "nix path-info -Sh /run/current-system";
      ytmp3 = ''
              ${lib.getExe yt-dlp} -x --continue --add-metadata --embed-thumbnail --audio-format mp3 --audio-quality 0 --metadata-from-title="%(artist)s - %(title)s" --prefer-ffmpeg -o "%(title)s.%(ext)s"'';
      cat = "${lib.getExe bat} --style=plain";
      grep = lib.getExe ripgrep;
      du = lib.getExe du-dust;
      ps = lib.getExe procs;
      m = "mkdir -p";
      fcd = "cd $(find -type d | fzf)";
      ls = "${lib.getExe exa} -h --git --icons --color=auto --group-directories-first -s extension";
      l = "ls -lF --time-style=long-iso --icons";
      la = "${lib.getExe exa} -lah --tree";
      tree = "${lib.getExe exa} --tree --icons --tree";
      http = "${lib.getExe python3} -m http.server";
      burn = "pkill -9";
      diff = "diff --color=auto";
      kys = "doas shutdown now";
      killall = "pkill";
      ".1" = "cd ..";
      ".2" = "cd ../..";
      ".3" = "cd ../../..";
      c = "clear";

      v = "nvim";
      emd = "pkill emacs; emacs --daemon";

      e = "emacsclient -t";
      cp="cp -iv";
      mv="mv -iv";
      rm="rm -vI";
      bc="bc -ql";
      mkd="mkdir -pv";
      ytfzf="ytfzf -Df";
      hyprcaps="hyprctl keyword input:kb_options caps:caps";
      gc = "git clone --depth=1";
      sudo = "doas";
    };
    plugins = with pkgs; [
      {
        name = "autopair.fish";
        src = pkgs.fishPlugins.autopair-fish;
      }
    ];
  };

  zsh = {
    enable = true;
    dotDir = ".config/shell";
    sessionVariables = {
      LC_ALL = "en_US.UTF-8";
      ZSH_AUTOSUGGEST_USE_ASYNC = "true";
      BEMENU_OPTS = "-i -l 10 -p '  Apps : ' -c -B 2 -W 0.5 --hp 15 --fn 'ComicCodeLigatures 20' --nb '#00000099' --ab '#00000099' --bdr '#c6daff' --nf '#ffffff' --af '#ffffff' --hb '#fff0f5' --hf '#000000' --fb '#00000099' --ff '#a6e3a1' --tb '#00000099' --tf '#f9e2af' ";
      NIXOS_OZONE_WL = "1";
      BROWSER = "librewolf";
      MOZ_ENABLE_WAYLAND = "1";
    };

    completionInit = ''
          eval "$(starship init zsh)"

          autoload -U compinit && compinit

          zstyle ':completion:*' list-colors "''${(s.:.)LS_COLORS}"

          # preview directory's content with exa when completing cd
          zstyle ':fzf-tab:complete:*:*' fzf-preview 'exa -1 --color=always $realpath'

          # switch group using `,` and `.`
          zstyle ':fzf-tab:*' switch-group ',' '.'

          # give a preview of commandline arguments when completing `kill`
          zstyle ':completion:*:*:*:*:processes' command "ps -u $USER -o pid,user,comm -w -w"
          zstyle ':fzf-tab:complete:(kill|ps):argument-rest' fzf-preview \
            '[[ $group == "[process ID]" ]] && ps --pid=$word -o cmd --no-headers -w -w'
            zstyle ':fzf-tab:complete:(kill|ps):argument-rest' fzf-flags --preview-window=down:3:wrap

          bindkey -e
        '';

    envExtra = ''
    export MANPAGER="sh -c 'col -bx | bat -l man -p'"
    export PATH="$PATH:$HOME/.DLIP/BIN"
    export PATH="$PATH:$HOME/.local/bin/d"
    export EDITOR="emacsclient -nw -a 'nvim'"
    export VISUAL=$EDITOR
    export STARDICT_DATA_DIR="$HOME/.local/share/stardict"

    export GRIM_DEFAULT_DIR="$HOME/pics/sshots/"

        if [ -z $DISPLAY ] && [ "$(tty)" = "/dev/tty1" ]; then
          exec Hyprland
        fi

        '';
    initExtra = ''
        source "${pkgs.zsh-syntax-highlighting}/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
        source "${pkgs.zsh-history-substring-search}/share/zsh-history-substring-search/zsh-history-substring-search.zsh"
        source "${pkgs.zsh-autosuggestions}/share/zsh-autosuggestions/zsh-autosuggestions.zsh"
        source "${pkgs.zsh-autopair}/share/zsh/zsh-autopair/autopair.zsh"
        source "${pkgs.nix-zsh-completions}/share/zsh/plugins/nix/nix-zsh-completions.plugin.zsh"
        source "${pkgs.fzf}/share/fzf/completion.zsh"
        source "${pkgs.fzf}/share/fzf/key-bindings.zsh"
        source "${pkgs.zsh-fzf-tab}/share/fzf-tab/fzf-tab.plugin.zsh"
        source "${pkgs.zsh-fzf-tab}/share/fzf-tab/lib/zsh-ls-colors/ls-colors.zsh"

          function ytdl() {
              yt-dlp --embed-metadata --embed-subs -f 22 "$1"
          }

        '';

    history = {
      save = 1000;
      size = 1000;
      expireDuplicatesFirst = true;
      ignoreDups = true;
    };

    shellAliases = let
      # for setting up license in new projects

    in
      with pkgs; {
        rebuild = "doas nix-store --verify; pushd ~dotfiles && doas nixos-rebuild switch --flake .# && notify-send \"Done\"&& bat cache --build; popd";
        cleanup = "doas nix-collect-garbage --delete-older-than 7d";
        bloat = "nix path-info -Sh /run/current-system";
        ytmp3 = ''
              ${lib.getExe yt-dlp} -x --continue --add-metadata --embed-thumbnail --audio-format mp3 --audio-quality 0 --metadata-from-title="%(artist)s - %(title)s" --prefer-ffmpeg -o "%(title)s.%(ext)s"'';
        cat = "${lib.getExe bat} --style=plain";
        grep = lib.getExe ripgrep;
        du = lib.getExe du-dust;
        ps = lib.getExe procs;
        m = "mkdir -p";
        fcd = "cd $(find -type d | fzf)";
        ls = "${lib.getExe exa} -h --git --icons --color=auto --group-directories-first -s extension";
        l = "ls -lF --time-style=long-iso --icons";
        la = "${lib.getExe exa} -lah --tree";
        tree = "${lib.getExe exa} --tree --icons --tree";
        http = "${lib.getExe python3} -m http.server";
        burn = "pkill -9";
        diff = "diff --color=auto";
        kys = "doas shutdown now";
        killall = "pkill";
        ".1" = "cd ..";
        ".2" = "cd ../..";
        ".3" = "cd ../../..";
        c = "clear";

        v = "nvim";
        emd = "pkill emacs; emacs --daemon";

        e = "emacsclient -t";
        cp="cp -iv";
        mv="mv -iv";
        rm="rm -vI";
        bc="bc -ql";
        mkd="mkdir -pv";
        ytfzf="ytfzf -Df";
        hyprcaps="hyprctl keyword input:kb_options caps:caps";
        gc = "git clone --depth=1";
        sudo = "doas";
      };
  };
};

home.packages = with pkgs; [

# wayland
libnotify libsixel wofi seatd bemenu
  wf-recorder brightnessctl pamixer slurp grim
  wl-clipboard rofi-wayland cliphist wtype swaybg swayidle gammastep

# media
mpc_cli playerctl pavucontrol pulsemixer imv

# cli tools
cached-nix-shell pcmanfm yt-dlp fzf neovim btop

  unzip aspell aspellDicts.en hunspell enchant
  ripgrep nitch libreoffice transmission pandoc
  rsync  ffmpeg sdcv imagemagick groff
  # texlive.combined.scheme-full
  fd ncdu mu isync ts  syncthing 
  jq keepassxc figlet keepassxc dconf gcc

# themes
gruvbox-gtk-theme
  orchis-theme
  bibata-cursors
  papirus-icon-theme

# dl media
deluged yt-dlp jq ytfzf ani-cli

# pioneer of web
firefox librewolf brave ungoogled-chromium hugo

nodePackages_latest.bash-language-server
  nodePackages_latest.vscode-langservers-extracted
  # python39Packages.python-lsp-server
  python3
  marksman nil 
  tree-sitter-grammars.tree-sitter-bash

];

gtk = {
  enable = true;
  theme = {
    name = "Gruvbox-Dark-B";
  };
  iconTheme = {
    name = "Papirus";
  };
  font = {
    name = "ComicCodeLigatures";
    size = 17;
  };
  gtk3.extraConfig = {
    gtk-xft-antialias = 1;
    gtk-xft-hinting = 1;
    gtk-xft-hintstyle = "hintslight";
    gtk-xft-rgba = "rgb";
  };
  gtk2.extraConfig = ''
    gtk-xft-antialias=1
    gtk-xft-hinting=1
    gtk-xft-hintstyle="hintslight"
    gtk-xft-rgba="rgb"
  '';
};

# cursor theme
home.pointerCursor = {
  name = "Bibata-Modern-Classic";
  package = pkgs.bibata-cursors;
  size = 24;
  gtk.enable = true;
};

programs = {
  aria2 = {
    enable = true;
    settings = {
      dir = "/home/i/dloads";
      file-allocation = "none";
      log-level = "warn";
      split = "10";
      max-connection-per-server = 6;
      min-split-size = "5M";
      on-download-complete = "exit";
    };
  };
};

programs.bottom = {
  enable = true;
  settings = {
    flags.group_processes = true;
    row = [
      {
        ratio = 2;
        child = [
          {type = "cpu";}
          {type = "mem";}
        ];
      }
      {
        ratio = 3;
        child = [
          {
            type = "proc";
            ratio = 1;
            default = true;
          }
        ];
      }
    ];
  };
};

services.dunst = {
  enable = true;
  iconTheme = {
    package = pkgs.papirus-icon-theme;
    name = "Papirus";
  };
  settings = {
    global = {
      monitor = 0;
      background = "#1e1e2e";
      frame_color = "#89AAEB";
      transparency = 0;
      follow = "none";
      width = 900;
      height = 900;
      idle_threshold = 120;
      origin = "top-right";
      offset = "10x50";
      scale = 0;
      notification_limit = 0;
      progress_bar = "true";
      alignment = "center";
      progress_bar_height = 10;
      progress_bar_frame_width = 1;
      progress_bar_min_width = 150;
      progress_bar_max_width = 500;
      indicate_hidden = "yes";
      separator_height = 2;
      padding = 20;
      horizontal_padding = 12;
      text_icon_padding = 8;
      frame_width = 3;
      separator_color = "frame";
      sort = "yes";
      font = "ComicCodeLigatures 20";
      line_height = 0;
      markup = "full";
      stack_duplicates = "true";
      vertical_alignment = "center";
      show_age_threshold = 60;
      ellipsize = "middle";
      ignore_newline = "no";
      show_indicators = "yes";
      icon_position = "left";
      max_icon_size = 32;
      sticky_history = "yes";
      history_length = 20;
      browser = "/home/i/.DLIP/BIN/d-stuff";
      always_run_script = "true";
      title = "Dunst";
      class = "Dunst";
      corner_radius = 20;
      ignore_dbusclose = false;
      force_xwayland = "false";
      layer = "overlay";
      mouse_left_click = "do_action";
      mouse_middle_click = "do_action";
      mouse_right_click = "close_all";
      };

      reminder = {
       category = "reminder";
       background = "#33333390";
       foreground = "#ffffff";
       timeout = 0;
       script="d-notif";
    };

    urgency_low = {
      background = "#1e1e2e";
      foreground = "#cdd6f4";
      timeout = 5;
    };
    urgency_normal = {
      background = "#1e1e2e";
      foreground = "#cdd6f4";
      timeout = 6;
    };
    urgency_critical = {
      background = "#1e1e2e";
      foreground = "#cdd6f4";
      frame_color = "#f38ba8";
      timeout = 0;
    };
  };
};

programs.foot = {
  enable = true;
  # doesnt work properly
  server.enable = false;
  settings = {
    main = {
      term = "xterm-256color";
      font = "ComicCodeLigatures:size=12";
      font-bold = "Fira Code Nerd Font:size=12";
      letter-spacing = "1";
       box-drawings-uses-font-glyphs = "no";
    };
    scrollback = {
      lines = "1000";
    };
    key-bindings = {
      clipboard-copy = "Control+Shift+c Control+w";
      clipboard-paste = "Control+Shift+v Control+y";
      primary-paste = "Shift+Insert";

    };
    colors = {
     background="000000";
     foreground="ffffff";
     regular0="000000";
     regular1="ff8059";
     regular2="44bc44";
     regular3="d0bc00";
     regular4="2fafff";
     regular5="feacd0";
     regular6="00d3d0";
     regular7="bfbfbf";
     bright0="595959";
     bright1="ef8b50";
     bright2="70b900";
     bright3="c0c530";
     bright4="79a8ff";
     bright5="b6a0ff";
     bright6="6ae4b9";
     bright7="ffffff";
      alpha= "0.8";
    };
    mouse = {
      hide-when-typing = "yes";
    };
  };
};

wayland.windowManager.hyprland = {
  enable = true;
};

  xdg.configFile."hypr/hyprland.conf".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/.DLIP/SETUP/gdk/i-home/configs/hyprland.conf";

wayland.windowManager.sway = {
  enable = true;
  config = null;
  extraConfig = ''
    # user config directory
include $HOME/.config/sway/config.d/*

# only enable this if every app you use is compatible with wayland
xwayland disable
         '';
   wrapperFeatures.gtk = true;
};

home.file.".config/emacs/early-init.el".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/.DLIP/SETUP/gdk/i-home/configs/early-init.el";
home.file.".config/emacs/init.el".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/.DLIP/SETUP/gdk/i-home/configs/init.el";
home.file.".config/emacs/elfeed.org".source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/.DLIP/SETUP/d-rss.org";

programs.emacs = {
  enable = true;
  package = pkgs.emacsPgtk;
  extraPackages = (epkgs: (with epkgs; [
    vterm undo-tree flycheck helpful ox-pandoc
    no-littering rainbow-delimiters rainbow-mode vertico 
    orderless consult marginalia embark embark-consult org olivetti org-modern corfu
    cape markdown-mode nix-mode rust-mode lua-mode
    all-the-icons all-the-icons-dired async dired-hide-dotfiles dired-single
    reddigg mingus pdf-tools which-key magit aria2 webpaste org-present
    org-mime corfu-terminal beframe denote tempel tempel-collection
    sdcv elfeed elfeed-org link-hint powerthesaurus 
    doom-modeline hide-mode-line org-auto-tangle el-fetch ox-hugo htmlize
  ])
  );
};

programs.firefox = {
  enable = true;

profiles.ihome = {
  isDefault = true;
  # extensions = with pkgs.nur.repos.rycee.firefox-addons; [
  #   #bypass-paywalls-clean
  #   cookies-txt
  #   ff2mpv
  #   vimium
  #   languagetool
  #   ublock-origin
  #   darkreader
  #   libredirect
  #   multi-account-containers
  # ];

settings = {
  "app.update.auto" = false;
  "browser.startup.homepage" = "about:blank";
  "browser.urlbar.placeholderName" = "DuckDuckGo";
  "privacy.webrtc.legacyGlobalIndicator" = true;
  "gfx.webrender.all" = true;
  "gfx.webrender.enabled" = true;
  "media.av1.enabled" = false;
  "media.ffmpeg.vaapi.enabled" = true;
  "media.hardware-video-decoding.force-enabled" = true;
  "media.navigator.mediadatadecoder_vpx_enabled" = true;
  "signon.rememberSignons" = false;
  "app.normandy.api_url" = "";
  "app.normandy.enabled" = false;
  "app.shield.optoutstudies.enabled" = false;
  "beacon.enabled" = false;
  "breakpad.reportURL" = "";
  "browser.aboutConfig.showWarning" = false;
  "browser.cache.offline.enable" = false;
  "browser.crashReports.unsubmittedCheck.autoSubmit" = false;
  "browser.crashReports.unsubmittedCheck.autoSubmit2" = false;
  "browser.crashReports.unsubmittedCheck.enabled" = false;
  "browser.disableResetPrompt" = true;
  "browser.newtab.preload" = false;
  "browser.newtabpage.activity-stream.section.highlights.includePocket" = false;
  "extensions.pocket.enabled" = false;

  "browser.newtabpage.enhanced" = false;
  "browser.newtabpage.introShown" = true;
  "browser.safebrowsing.appRepURL" = "";
  "browser.safebrowsing.blockedURIs.enabled" = false;
  "browser.safebrowsing.downloads.enabled" = false;
  "browser.safebrowsing.downloads.remote.enabled" = false;
  "browser.safebrowsing.downloads.remote.url" = "";
  "browser.safebrowsing.enabled" = false;
  "browser.safebrowsing.malware.enabled" = false;
  "browser.safebrowsing.phishing.enabled" = false;
  "browser.selfsupport.url" = "";
  "browser.send_pings" = false;
  "browser.sessionstore.privacy_level" = 2;
  "browser.startup.homepage_override.mstone" = "ignore";
  "browser.tabs.crashReporting.sendReport" = false;
  "browser.urlbar.groupLabels.enabled" = false;
  "browser.urlbar.quicksuggest.enabled" = false;
  "browser.urlbar.speculativeConnect.enabled" = false;
  "datareporting.healthreport.service.enabled" = false;
  "datareporting.healthreport.uploadEnabled" = false;
  "datareporting.policy.dataSubmissionEnabled" = false;
  "device.sensors.ambientLight.enabled" = false;
  "device.sensors.enabled" = false;
  "device.sensors.motion.enabled" = false;
  "device.sensors.orientation.enabled" = false;
  "device.sensors.proximity.enabled" = false;
  "dom.battery.enabled" = false;
  "dom.event.clipboardevents.enabled" = true;
  "dom.webaudio.enabled" = false;
  "experiments.activeExperiment" = false;
  "experiments.enabled" = false;
  "experiments.manifest.uri" = "";
  "experiments.supported" = false;
  "extensions.getAddons.cache.enabled" = false;
  "extensions.getAddons.showPane" = false;
  "extensions.greasemonkey.stats.optedin" = false;
  "extensions.greasemonkey.stats.url" = "";
  "extensions.shield-recipe-client.api_url" = "";
  "extensions.shield-recipe-client.enabled" = false;
  "extensions.webservice.discoverURL" = "";
  "fission.autostart" = true;
  "media.autoplay.default" = 1;
  "media.autoplay.enabled" = false;
  "media.eme.enabled" = false;
  "media.gmp-widevinecdm.enabled" = false;
  "media.navigator.enabled" = false;
  "media.video_stats.enabled" = false;
  "network.IDN_show_punycode" = true;
  "network.allow-experiments" = false;
  "network.captive-portal-service.enabled" = false;
  "network.cookie.cookieBehavior" = 1;
  "network.dns.disablePrefetch" = true;
  "network.dns.disablePrefetchFromHTTPS" = true;
  "network.http.referer.spoofSource" = true;
  "network.http.speculative-parallel-limit" = 0;
  "network.predictor.enable-prefetch" = false;
  "network.predictor.enabled" = false;
  "network.prefetch-next" = false;
  "network.trr.mode" = 5;
  "privacy.donottrackheader.enabled" = true;
  "privacy.donottrackheader.value" = 1;
  "privacy.firstparty.isolate" = true;
  "privacy.trackingprotection.cryptomining.enabled" = true;
  "privacy.trackingprotection.enabled" = true;
  "privacy.trackingprotection.fingerprinting.enabled" = true;
  "privacy.trackingprotection.pbmode.enabled" = true;
  "privacy.usercontext.about_newtab_segregation.enabled" = true;
  "security.ssl.disable_session_identifiers" = true;
  "services.sync.prefs.sync.browser.newtabpage.activity-stream.showSponsoredTopSite" = false;
  "browser.newtabpage.activity-stream.showSponsored" = false;
  "signon.autofillForms" = false;
  "toolkit.telemetry.archive.enabled" = false;
  "toolkit.telemetry.bhrPing.enabled" = false;
  "toolkit.telemetry.cachedClientID" = "";
  "toolkit.telemetry.enabled" = false;
  "toolkit.telemetry.firstShutdownPing.enabled" = false;
  "toolkit.telemetry.hybridContent.enabled" = false;
  "toolkit.telemetry.newProfilePing.enabled" = false;
  "toolkit.telemetry.prompted" = 2;
  "toolkit.telemetry.rejected" = true;
  "toolkit.telemetry.reportingpolicy.firstRun" = false;
  "toolkit.telemetry.server" = "";
  "toolkit.telemetry.shutdownPingSender.enabled" = false;
  "toolkit.telemetry.unified" = false;
  "toolkit.telemetry.unifiedIsOptIn" = false;
  "toolkit.telemetry.updatePing.enabled" = false;
  "webgl.disabled" = true;
  "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
  "webgl.renderer-string-override" = " ";
  "webgl.vendor-string-override" = " ";
};

userChrome = ''
	     /* Base color for the theme, dependent on whether it's a light theme or not */
@media (prefers-color-scheme: dark) {
    :root {
	--lwt-accent-color: #1c1b22;
    }
}

@media (prefers-color-scheme: light) {
    :root {
	--lwt-accent-color: #FAFAFC;
    }
}

/*====== Aesthetics ======*/

#navigator-toolbox {
    border-bottom: none !important;
}

#titlebar {
    background: var(--lwt-accent-color) !important;
}

/* Sets the toolbar color */
toolbar#nav-bar {
    background: var(--lwt-accent-color) !important;
    box-shadow: none !important;
}

/* Sets the URL bar color */
#urlbar {
    background: var(--lwt-accent-color) !important;
}

#urlbar-background {
    background: var(--lwt-accent-color) !important;
    border: none !important;
}

#urlbar-input-container {
    border: none !important;
}

/*====== UI Settings ======*/

:root {
    --navbarWidth: 475px; /* Set width of navbar */
}

/* If the window is wider than 1000px, use flex layout */
@media (min-width: 1000px) {
    #navigator-toolbox {
	display: flex;
	flex-wrap: wrap;
}

    /*  Url bar  */
    #nav-bar {
	order: 1;
	width: var(--navbarWidth);
    }

    /* Tab bar */
    #titlebar {
	order: 2;
	width: calc(100vw - var(--navbarWidth) - 1px);
    }

    /* Bookmarks bar */
    #PersonalToolbar {
	order: 3;
	width: 100%;
    }

    /* Fix urlbar sometimes being misaligned */
    :root[uidensity="compact"] #urlbar {
	--urlbar-toolbar-height: 39.60px !important;
    }

    :root[uidensity="touch"] #urlbar {
	--urlbar-toolbar-height: 49.00px !important;
    }
}

/*====== Simplifying interface ======*/

/* Autohide back button when disabled */
#back-button, #forward-button {
    transform: scale(1, 1) !important;
    transition: margin-left 150ms var(--animation-easing-function), opacity 250ms var(--animation-easing-function), transform 350ms var(--animation-easing-function) !important;
}

#back-button[disabled="true"], #forward-button[disabled="true"] {
    margin-left: -34px !important;
    opacity: 0 !important;
    transform: scale(0.8, 0.8) !important;
    pointer-events: none !important;
}

/* Remove UI elements */
#identity-box, /* Site information */
#tracking-protection-icon-container, /* Shield icon */
#page-action-buttons > :not(#urlbar-zoom-button, #star-button-box), /* All url bar icons except for zoom level and bookmarks */
#urlbar-go-button, /* Search URL magnifying glass */
#alltabs-button, /* Menu to display all tabs at the end of tabs bar */
.titlebar-buttonbox-container /* Minimize, maximize, and close buttons */ {
    display: none !important;
}

#nav-bar {
    box-shadow: none !important;
}

/* Remove "padding" left and right from tabs */
.titlebar-spacer {
    display: none !important;
}

/* Fix URL bar overlapping elements */
#urlbar-container {
    min-width: initial !important;
}

/* Remove gap after pinned tabs */
#tabbrowser-tabs[haspinnedtabs]:not([positionpinnedtabs])
> #tabbrowser-arrowscrollbox
> .tabbrowser-tab[first-visible-unpinned-tab] {
    margin-inline-start: 0 !important;
}

/* Hide the hamburger menu */
#PanelUI-menu-button { 
    padding: 0px !important;
}

#PanelUI-menu-button .toolbarbutton-icon { 
    width: 1px !important; 
}

#PanelUI-menu-button .toolbarbutton-badge-stack {
    padding: 0px !important; 
}
	     '';

};

};

services.mpd = {
  enable = true;
  network = {
    listenAddress = "any";
    port = 6600;
  };
  extraConfig = ''
      audio_output {
        type    "pipewire"
        name    "pipewire"
      }
      auto_update "yes"
    '';
};

programs.ncmpcpp = {
  enable = true;
};

programs = {
  mpv = {
    enable = true;
    scripts = with pkgs.mpvScripts; [ thumbnail sponsorblock];
  };
};

programs.sioyek = {
  enable = true;
};

services = {
  # udiskie = {
  # enable = true;
  # automount = true;
  # };
  gpg-agent = {
    enable = true;
    pinentryFlavor = "gnome3";
    enableSshSupport = true;
    enableZshIntegration = true;
  };
};
programs = {
  gpg.enable = true;
  man.enable = true;
  #  direnv = {
  #    enable = true;
  #    nix-direnv.enable = true;
  #  };
  #  tealdeer = {
  #    enable = true;
  #    settings = {
  #      display = {
  #        compact = false;
  #        use_pager = true;
  #      };
  #      updates = {
  #        auto_update = true;
  #      };
  #    };
  #  };
  bat = {
    enable = true;
  };
};
xdg = {
  userDirs = {
    enable = true;
    documents = "$HOME/docs";
    download = "$HOME/dloads";
    videos = "$HOME/vids";
    music = "$HOME/music";
    pictures = "$HOME/pics";
    desktop = "$HOME/other";
    publicShare = "$HOME/other";
    templates = "$HOME/other";
  };
  mimeApps.enable = true;
  mimeApps.associations.added = {
    "text/html" = ["firefox.desktop"];
    "x-scheme-handler/http" = ["firefox.desktop"];
    "x-scheme-handler/https" = ["firefox.desktop"];
    "x-scheme-handler/ftp" = ["firefox.desktop"];
    "x-scheme-handler/about" = ["firefox.desktop"];
    "x-scheme-handler/unknown" = ["firefox.desktop"];
    "application/x-extension-htm" = ["firefox.desktop"];
    "application/x-extension-html" = ["firefox.desktop"];
    "application/x-extension-shtml" = ["firefox.desktop"];
    "application/xhtml+xml" = ["firefox.desktop"];
    "application/x-extension-xhtml" = ["firefox.desktop"];
    "application/x-extension-xht" = ["firefox.desktop"];

    "audio/*" = ["mpv.desktop"];
    "video/*" = ["mpv.dekstop"];
    "image/*" = ["imv.desktop"];
    "application/json" = ["firefox.desktop"];
    "application/pdf" = ["sioyek.desktop"];
    "x-scheme-handler/magnet" = ["d-stuff.desktop"];
    "application/epub+zip" = ["sioyek.desktop"];
    "application/zip" = ["sioyek.desktop"];
    "application/x.bittorrent" = ["d-stuff.desktop"];
  };
  mimeApps.defaultApplications = {
    "text/html" = ["firefox.desktop"];
    "x-scheme-handler/http" = ["firefox.desktop"];
    "x-scheme-handler/https" = ["firefox.desktop"];
    "x-scheme-handler/ftp" = ["firefox.desktop"];
    "x-scheme-handler/about" = ["firefox.desktop"];
    "x-scheme-handler/unknown" = ["firefox.desktop"];
    "application/x-extension-htm" = ["firefox.desktop"];
    "application/x-extension-html" = ["firefox.desktop"];
    "application/x-extension-shtml" = ["firefox.desktop"];
    "application/xhtml+xml" = ["firefox.desktop"];
    "application/x-extension-xhtml" = ["firefox.desktop"];
    "application/x-extension-xht" = ["firefox.desktop"];

    "audio/*" = ["mpv.desktop"];
    "video/*" = ["mpv.dekstop"];
    "image/*" = ["imv.desktop"];
    "application/json" = ["firefox.desktop"];
    "application/pdf" = ["sioyek.desktop"];
    "x-scheme-handler/magnet" = ["d-stuff.desktop"];
    "application/epub+zip" = ["sioyek.desktop"];
    "application/zip" = ["sioyek.desktop"];
    "application/x.bittorrent" = ["d-stuff.desktop"];
  };
};

programs.waybar = {
  enable = true;
  package = pkgs.waybar.overrideAttrs (oldAttrs: {
    mesonFlags = oldAttrs.mesonFlags ++ ["-Dexperimental=true"];
  });

  settings = {
    mainBar = {
      layer = "top";
      position = "top";
      height = 20;
      spacing = 7;
      fixed-center = true;
      exclusive = true;
      modules-left = [
        "custom/launcher"
        "wlr/workspaces"
        "hyprland/window"
        "hyprland/submap"
      ];
      modules-center = [
        "clock"
      ];
      modules-right = ["network" "battery" "memory" "pulseaudio" "custom/power"];
      "wlr/workspaces" = {
        format = "{icon}";
        active-only = false;
        on-click = "activate";
        format-icons = {
          active = "";
          default = "";
          "1" = "1";
          "2" = "2";
          "3" = "3";
          "4" = "4";
          "5" = "5";
          "6" = "6";
        };
      };

      "hyprland/window" = {
        "format" = "🧬 {}";
        "separate-outputs" = true;
      };

      "hyprland/submap" = {
        "format" = " {}";
        "max-length" = 14;
        "tooltip" = false;
      };

      "custom/launcher" = {
        format = " ";
        tooltip = false;
        on-click = "bemenu-run";
      };

      "battery" =  {
        "bat" =  "BAT0";
        "interval" =  30;
        "states" =  {
          "warning" =  50;
          "critical" =  30;
        };
        "format" =  "{capacity}% {icon} ";
        "format-icons" =  ["" "" "" "" ""];
        "max-length" =  25;
      };

      "custom/power" = {
        "format" = " ";
        "on-click" = "d-power";
        "tooltip" = false;
      };
      "clock" = {
        "tooltip-format" = "{:%A %B %d %Y | %H:%M}";
        "format-alt" = " {:%a %d %b  %I:%M %p}";
        "format" = " {:%H:%M} ";
        ##"timezones" = [ "Kolkata" ];
        ##"max-length" = 200;
        "interval" = 1;
      };
      "cpu" = {
        "format" = "﬙ {usage: >3}%";
        "on-click" = "footclient -e htop";
      };
      "memory" = {
        "format" = " {: >3}%";
        "on-click" = "foot -e btop";
      };
      "network" = {
        "interface" = "wlp2s0";
        "format" = "⚠ Disabled";
        "format-wifi" = " {bandwidthDownBytes}  {bandwidthUpBytes}";
        "format-ethernet" = " {ifname}: {ipaddr}/{cidr}";
        "format-disconnected" = "⚠ Disconnected";
        "on-click" = "foot -e nmtui";
        "interval" = 2;
      };
      "pulseaudio" = {
        "scroll-step" = 2;
        "format" = "{icon} {volume: >3}%";
        "format-bluetooth" = "{icon} {volume: >3}%";
        "format-muted" =" muted";
        "on-click" = "pamixer -t";
        "format-icons" = {
          "headphones" = "";
          "handsfree" = "";
          "headset" = "";
          "phone" = "";
          "portable" = "";
          "car" = "";
          "default" = ["" ""];
        };
      };
    };
  };
};

}
