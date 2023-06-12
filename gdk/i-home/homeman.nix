# WARNING : This file was generated by d-setup.org
# DO NOT MODIFY THIS FILE!
# Any changes made here will be overwritten.

{ inputs, pkgs, self, lib, config, ... }:

{

home.sessionVariables.STARSHIP_CACHE = "${config.xdg.cacheHome}/starship";
programs = {
  # Starship
  starship = {
    enable = false;
    enableZshIntegration = true;
    settings = {
      add_newline = true;
      scan_timeout = 5;
      character = {
        error_symbol = " [](#df5b61)";
        success_symbol = "[](#6791c9)";
        vicmd_symbol = "[](bold yellow)";
        format = "[  ](bold blue)[$directory$all$character](bold)[  ](bold green)";
      };
      git_commit = {commit_hash_length = 4;};
      line_break.disabled = false;
      lua.symbol = "[󰢱](blue) ";
      python.symbol = "[](blue) ";
      directory.read_only = " ";
      nix_shell.symbol = " ";
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
      ytfzf="ytfzf -D";
      hyprcaps="hyprctl keyword input:kb_options caps:caps";
      gc = "git clone --depth=1";
      sudo = "doas";
    };
  };

  zsh = {
    enable = true;
    dotDir = ".config/shell";
    enableCompletion = true;
    enableAutosuggestions = true;
    history.extended = true;
    sessionVariables = {
      LC_ALL = "en_US.UTF-8";
      ZSH_AUTOSUGGEST_USE_ASYNC = "true";
      NIXOS_OZONE_WL = "1";
      BROWSER = "librewolf";
      MOZ_ENABLE_WAYLAND = "1";
    };

    envExtra = ''
        export MANPAGER="sh -c 'col -bx | bat -l man -p'"
        export PATH="$PATH:/home/i/d-git/d-bin/bin:$HOME/.local/bin/d"
        export EDITOR="emacsclient -nw -a 'nvim'"
        export BEMENU_OPTS="-i -s -l 10 -p ' ' -c -B 2 -W 0.5 --hp 15 --fn 'ComicCodeLigatures Nerd Font 20' --nb '#121212' --ab '#121212' --bdr '#c6daff' --nf '#ffffff' --af '#ffffff' --hb '#9aff9a' --hf '#121212' --fb '#121212' --ff '#a6e3a1' --tb '#121212' --tf '#f9e2af' ";
        export VISUAL=$EDITOR
        export STARDICT_DATA_DIR="$HOME/.local/share/stardict"
        export GRIM_DEFAULT_DIR="$HOME/pics/sshots/"

        if [ -z $DISPLAY ] && [ "$(tty)" = "/dev/tty1" ]; then
          exec Hyprland
        fi
        '';

    initExtra = lib.mkAfter ''
        source /home/i/d-git/d-nix/gdk/i-home/configs/sources.sh

        source "${pkgs.zsh-syntax-highlighting}/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
        source "${pkgs.zsh-history-substring-search}/share/zsh-history-substring-search/zsh-history-substring-search.zsh"
        source "${pkgs.zsh-autosuggestions}/share/zsh-autosuggestions/zsh-autosuggestions.zsh"
        source "${pkgs.nix-zsh-completions}/share/zsh/plugins/nix/nix-zsh-completions.plugin.zsh"
        source "${pkgs.zsh-nix-shell}/share/zsh-nix-shell/nix-shell.plugin.zsh"
        source "${pkgs.zsh-autopair}/share/zsh/zsh-autopair/autopair.zsh"

        # For vterm, needs to be at last!
        function vterm_prompt_end() {
            printf "\e]%s\e\\" "$1" "51;A$(whoami)@$(hostname):$(pwd)"
            }
            setopt PROMPT_SUBST
            PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'                       
        '';

    history = {
      save = 10000;
      size = 10000;
      expireDuplicatesFirst = true;
      ignoreDups = true;
    };
  };
};

home.packages = with pkgs; [

# wayland
libnotify libsixel bemenu hyprpicker
wf-recorder brightnessctl pamixer slurp grim 
wl-clipboard wtype swaybg swayidle gammastep

# media
mpc_cli pulsemixer imv
yt-dlp jq ytfzf ani-cli qbittorrent youtube-tui

# cli tools
pcmanfm fzf neovim btop unzip
aspell aspellDicts.en-science aspellDicts.en hunspell hunspellDicts.en-us
ripgrep nitch libreoffice pandoc newsboat mupdf
rsync ffmpeg sdcv imagemagick groff
wkhtmltopdf-bin
fd ncdu mu isync ts syncthing dconf
jq keepassxc figlet

# themes
gruvbox-gtk-theme
  orchis-theme
  bibata-cursors
  papirus-icon-theme

# pioneer of web
firefox librewolf brave ungoogled-chromium hugo

nodePackages.bash-language-server
  nodePackages.vscode-langservers-extracted
  # python311Packages.python-lsp-server
  nodePackages.pyright
  python3
  nil 
  tree-sitter
  texlive.combined.scheme-full

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
    name = "ComicCodeLigatures Nerd Font";
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
      max-connection-per-server = 10;
      min-split-size = "5M";
      bt-max-peers = "0";
      bt-request-peer-speed-limit = "0";
      max-overall-upload-limit = "512k";
      bt-external-ip = "127.0.0.1";
      dht-listen-port = "6882";
      enable-dht = "true";
      enable-peer-exchange = "true";
      listen-port = "6881";
      bt-force-encryption = "true";
      bt-min-crypto-level = "arc4";
      bt-require-crypto = "true";
      follow-torrent = "mem";
      seed-ratio = "100";
      seed-time = "0";
      socket-recv-buffer-size = "1M";
      event-poll = "epoll";
      realtime-chunk-checksum = "true";
      allow-overwrite = "true";
      always-resume = "true";
      auto-file-renaming = "false";
      continue = "true";
      rpc-save-upload-metadata = "false";
    };
  };
};

programs = {
  exa = {
    enable = true;
    extraOptions = ["--group-directories-first" "--header"];
    icons = true;
  };
  bat = {
    enable = true;
    extraPackages = with pkgs.bat-extras; [ batdiff batman batgrep batwatch ];
    config = {
      theme = "gruvbox-dark";
    };
  };
};

# We will tangle config files from git repo to home dir (Let nix manage the magics)
home.file.".config/btop/btop.conf".source = config.lib.file.mkOutOfStoreSymlink "/home/i/d-git/d-nix/gdk/i-home/configs/btop/btop.conf";

services.dunst = {
  enable = true;
  iconTheme = {
    package = pkgs.papirus-icon-theme;
    name = "Papirus";
  };
  settings = {
    global = {
      monitor = 0;
      background = "#050505";
      frame_color = "#2e8b57";
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
      font = "ComicCodeLigatures Nerd Font 20";
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
      browser = "/home/i/d-git/d-bin/bin/d-stuff";
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
      background = "#050505";
      foreground = "#ffffff";
      timeout = 5;
    };
    urgency_normal = {
      background = "#050505";
      foreground = "#ffffff";
      timeout = 6;
    };
    urgency_critical = {
      background = "#050505";
      foreground = "#ffffff";
      frame_color = "#f38ba8";
      timeout = 0;
    };
  };
};

programs.foot = {
  enable = true;
  # doesnt work properly; Enable it in hyprland or sway config
  server.enable = false;
  settings = {
    main = {
      term = "xterm-256color";
      font = "ComicCodeLigatures Nerd Font:size=12, Noto Color Emoji:size=15, JetBrainsMono Nerd Font:size=12";
      font-bold = "ComicCodeLigatures Nerd Font:size=12, Noto Color Emoji:size=15, JetBrainsMono Nerd Font:size=12";
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
     background="050505";
     foreground="ffffff";
     regular0="030303";
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
      alpha= "1.0";
    };
    mouse = {
      hide-when-typing = "yes";
    };
  };
};

# Symlinking the file (hyprland.conf) from the path of repo so we can edit and have immediate effects without requiring to rebuild
  xdg.configFile."hypr/hyprland.conf".source = config.lib.file.mkOutOfStoreSymlink "/home/i/d-git/d-nix/gdk/i-home/configs/hypr/hyprland.conf";

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

# Symlinking emacs configs, so we can edit it in realtime and have immediate effect without requiring a rebuild.

home.file.".config/emacs/early-init.el".source = config.lib.file.mkOutOfStoreSymlink "/home/i/d-git/d-nix/gdk/i-home/configs/emacs/early-init.el";
home.file.".config/emacs/init.el".source = config.lib.file.mkOutOfStoreSymlink "/home/i/d-git/d-nix/gdk/i-home/configs/emacs/init.el";
home.file.".config/emacs/elfeed.org".source = config.lib.file.mkOutOfStoreSymlink "/home/i/d-git/d-nix/d-rss.org";

programs.emacs = {
  enable = true;
  package = pkgs.emacs29-pgtk;
  extraPackages = (epkgs: (with epkgs; [
    treesit-grammars.with-all-grammars
    vterm multi-vterm vundo undo-fu-session flycheck helpful ox-pandoc
    no-littering rainbow-delimiters rainbow-mode vertico 
    orderless consult marginalia embark org olivetti org-modern corfu
    embark-consult consult-eglot consult-flycheck
    cape markdown-mode nix-mode
    nerd-icons async dirvish
    reddigg hnreader mingus which-key magit webpaste org-present
    # pdf-tools nov
    shrface shr-tag-pre-highlight gcmh
    org-mime corfu-terminal beframe denote tempel tempel-collection
    sdcv elfeed elfeed-org link-hint powerthesaurus jinx meow
    doom-modeline hide-mode-line el-fetch ox-hugo htmlize
    ement kind-icon speed-type
  ])
  );
};

# We will tangle config files from git repo to home dir (Let nix manage the magics)

home.file.".config/helix/config.toml".source = config.lib.file.mkOutOfStoreSymlink "/home/i/d-git/d-nix/gdk/i-home/configs/helix/config.toml";

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
  "browser.urlbar.placeholderName" = "Brain";
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

/* Check this for updated! https://github.com/rockofox/firefox-minima/blob/main/userChrome.css  */

/* User changable variables */

:root {
        --tab-font-size: 0.8em; /* Font size of the tab labels */
        --tab-font-family: -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Helvetica, Arial, sans-serif; /* The font used for the tab labels */
        --max-tab-width: none; /* The maximum width a tab in the tab bar can use. Set this to none for no limit */
        --show-titlebar-buttons: none; /* Hide the buttons (close/minimize/maximize) in the title bar. Required on some platforms (e.g macOS) to fully hide the title bar. `none` hides them, `block` shows them */
        --tab-height: 20px;
}

/* Minima Source Code. Here be dragons. */
/* Only change this if you know what you're doing */

.titlebar-buttonbox-container {
        display: var(--show-titlebar-buttons);
}

:root:not([customizing]) #TabsToolbar {
        margin-left: 1px !important;
        margin-right: 1px !important;
        border-radius: 0 !important;
        padding: 0 !important;
}
.tabbrowser-tab * {
        margin:0 !important;
        border-radius: 0 !important;
        font-family: var(--tab-font-family) !important;
}
.tabbrowser-tab {
        height: var(--tab-height);
        font-size: var(--tab-font-size) !important;
        min-height: 0 !important;
}
.tabbrowser-tab[fadein]:not([pinned]) {
        max-width: var(--max-tab-width) !important;
}
.tab-close-button, #firefox-view-button, #scrollbutton-up, .tab-secondary-label {
        display: none !important;
}
.new-tab-button {
        display: right !important;
}
.tab-icon-image {
        height: auto !important;
        width: calc(var(--tab-height) / 1.5) !important;
        margin-right: 4px !important;
}

#tabs-newtab-button, #titlebar spacer {
        display: none !important;
}

:root:not([customizing]) #nav-bar
{
        min-height : 2.5em       !important;
        height     : 2.5em       !important;
        margin     : 0 0 -2.5em  !important;
        z-index    : -1000       !important;
        opacity    : 0           !important;
}

:root:not([customizing]) #nav-bar:focus-within
{
        z-index    : 1000        !important;
        opacity    : 1           !important;
}

#nav-bar{
        border-inline: var(--uc-window-drag-space-width) solid var(--toolbar-bgcolor);
}
#new-tab-button, #alltabs-button, #scrollbutton-down, .tab-loading-burst{
        display: none;
}
#titlebar {
        overflow: none !important;
}

/* Source file https://github.com/MrOtherGuy/firefox-csshacks/tree/master/chrome/hide_tabs_with_one_tab.css made available under Mozilla Public License v. 2.0
See the above repository for updates as well as full license text. */

/* Makes tabs toolbar items zero-height initially and sets enlarge them to fill up space equal to tab-min-height set on tabs. Optionally use privatemode_indicator_as_menu_button.css to replace main menu icon with private browsing indicator while tabs are hidden. */
/* Firefox 65+ only */

:root[sizemode="normal"] #nav-bar{ --uc-window-drag-space-width: 20px }

#titlebar{ -moz-appearance: none !important; }
#TabsToolbar{ min-height: 0px !important }

#tabbrowser-tabs, #tabbrowser-tabs > .tabbrowser-arrowscrollbox, #tabbrowser-arrowscrollbox{ min-height: 0 !important; }

:root:not([customizing]) #tabbrowser-tabs .tabs-newtab-button,
:root:not([customizing]) #tabs-newtab-button,
:root:not([customizing]) #TabsToolbar-customization-target > .toolbarbutton-1,
:root:not([customizing]) #TabsToolbar .titlebar-button{
        -moz-appearance: none !important;
        height: 0px;
        padding-top: 0px !important;
        padding-bottom: 0px !important;
        -moz-box-align: stretch;
        margin: 0 !important;
}

.accessibility-indicator,
.private-browsing-indicator{ 
        height: unset !important;
}
.accessibility-indicator > hbox{ padding-block: 0 !important }

#tabbrowser-tabs tab:only-of-type {
        visibility: collapse !important;
}

/* Button re-styling */
#tabs-newtab-button:hover,
#tabbrowser-tabs .tabs-newtab-button:hover{ background-color: var(--toolbarbutton-hover-background) }

#tabs-newtab-button > .toolbarbutton-icon,
#tabbrowser-tabs .tabs-newtab-button > .toolbarbutton-icon{
        padding: 0 !important;
        transform: scale(0.6);
        background-color: transparent !important;
}
/* Extra top padding  in maximized window */
@media (-moz-os-version: windows-win10){
        :root[sizemode="maximized"] #navigator-toolbox{ padding-top:7px !important; }
}
/* Fix window controls not being clickable */
:root[tabsintitlebar] #toolbar-menubar[autohide="true"][inactive]{
        transition: height 0ms steps(1) 80ms;
}
#nav-bar{
        border-inline: var(--uc-window-drag-space-width) solid var(--toolbar-bgcolor);
}
#navigator-toolbox {
        appearance: toolbar !important; /* Pretty much anything except none */
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

programs= {
  ncmpcpp = {
    enable = true;
  };
  mpv = {
    enable = true;
    # scripts = with pkgs.mpvScripts; [ thumbnail sponsorblock];
  };
  yt-dlp = {
    enable = true;
    settings = {
      embed-thumbnail = true;
      embed-metadata = true;
      embed-subs = true;
      sub-langs = "all";
    };
  };
};

# We will tangle config files from git repo to home dir (Let nix manage the magics)

home.file.".config/mpv/mpv.conf".source = config.lib.file.mkOutOfStoreSymlink "/home/i/d-git/d-nix/gdk/i-home/configs/mpv/mpv.conf";

# We will tangle config files from git repo to home dir (Let nix manage the magics)

home.file.".config/mpv/input.conf".source = config.lib.file.mkOutOfStoreSymlink "/home/i/d-git/d-nix/gdk/i-home/configs/mpv/input.conf";

# We will tangle config files from git repo to home dir (Let nix manage the magics)

home.file.".config/imv/config".source = config.lib.file.mkOutOfStoreSymlink "/home/i/d-git/d-nix/gdk/i-home/configs/imv/config";

# We will tangle config files from git repo to home dir (Let nix manage the magics)

home.file.".config/fuzzel/fuzzel.ini".source = config.lib.file.mkOutOfStoreSymlink "/home/i/d-git/d-nix/gdk/i-home/configs/fuzzel/fuzzel.ini";

# We will tangle config files from git repo to home dir (Let nix manage the magics)

home.file.".config/rofi/theme.rasi".source = config.lib.file.mkOutOfStoreSymlink "/home/i/d-git/d-nix/gdk/i-home/configs/rofi/theme.rasi";

# We will tangle config files from git repo to home dir (Let nix manage the magics)

home.file.".config/rofi/config.rasi".source = config.lib.file.mkOutOfStoreSymlink "/home/i/d-git/d-nix/gdk/i-home/configs/rofi/config.rasi";

# We will tangle config files from git repo to home dir (Let nix manage the magics)

home.file.".config/rofi/dmoji.rasi".source = config.lib.file.mkOutOfStoreSymlink "/home/i/d-git/d-nix/gdk/i-home/configs/rofi/dmoji.rasi";

# We will tangle config files from git repo to home dir (Let nix manage the magics)

home.file.".config/rofi/smenu.rasi".source = config.lib.file.mkOutOfStoreSymlink "/home/i/d-git/d-nix/gdk/i-home/configs/rofi/smenu.rasi";

# We will tangle config files from git repo to home dir (Let nix manage the magics)

home.file.".config/rofi/list.rasi".source = config.lib.file.mkOutOfStoreSymlink "/home/i/d-git/d-nix/gdk/i-home/configs/rofi/list.rasi";

programs.sioyek = {
  enable = true;
};

# We will tangle config files from git repo to home dir (Let nix manage the magics)

home.file.".config/sioyek/prefs_user.config".source = config.lib.file.mkOutOfStoreSymlink "/home/i/d-git/d-nix/gdk/i-home/configs/sioyek/prefs_user.config";

# We will tangle config files from git repo to home dir (Let nix manage the magics)

home.file.".config/sioyek/keys_user.config".source = config.lib.file.mkOutOfStoreSymlink "/home/i/d-git/d-nix/gdk/i-home/configs/sioyek/keys_user.config";

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
  # direnv = {
  #   enable = true;
  #   nix-direnv.enable = true;
  # };
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
xdg = {
  userDirs = {
    enable = true;
    documents = "$HOME/docs";
    download = "$HOME/dloads";
    videos = "$HOME/vids";
    music = "$HOME/music";
    pictures = "$HOME/pics";
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
  # package = pkgs.waybar.overrideAttrs (oldAttrs: {
  #   mesonFlags = oldAttrs.mesonFlags ++ ["-Dexperimental=true"];
  # });

  settings = {
    mainBar = {
      layer = "top";
      position = "top";
      # height = 15;
      # spacing = 7;
      fixed-center = true;
      exclusive = true;
      modules-left = [
        "custom/launcher"
        "wlr/workspaces"
        "hyprland/window"
        "hyprland/submap"
      ];
      modules-center = [
        "clock" "mpd"
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
        "format" = "👁{}";
        "separate-outputs" = true;
      };

      "hyprland/submap" = {
        "format" = " {}";
        "max-length" = 14;
        "tooltip" = false;
      };

      "custom/launcher" = {
        "format" = " ";
        "tooltip" = false;
        "on-click" = "bemenu-run";
      };

      "battery" =  {
        "bat" =  "BAT0";
        "interval" =  30;
        "states" =  {
          "warning" =  50;
          "critical" =  30;
        };
        "format" =  "{capacity}% {icon} ";
        "format-icons" =  [" " "🔴 " "🪫" " " " "];
        "max-length" =  25;
      };

      "mpd" = {
        "format" = "{stateIcon} {title}  ";
        "format-disconnected" = "  ";
        "format-stopped" = "  ";
        "title-len" = 20;
        "interval" = 10;
        "on-click" = "mpc toggle";
        "state-icons" = {
          "paused" = "";
          "playing" = "";
        };
        "tooltip-format" = "Playing: {filename}";
          "tooltip-format-disconnected" = "";
      };
        "custom/power" = {
        "format" = "⏻";
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
        "on-click" = "footclient -e btop";
      };
      "memory" = {
        "format" = " {: >3}% ";
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
        "format" = "{icon} {volume: >3}% ";
        "format-bluetooth" = "{icon} {volume: >3}%";
        "format-muted" =" muted ";
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

# We will tangle config files from git repo to home dir (Let nix manage the magics)

home.file.".config/waybar/style.css".source = config.lib.file.mkOutOfStoreSymlink "/home/i/d-git/d-nix/gdk/i-home/configs/waybar/style.css";

# We will tangle config files from git repo to home dir (Let nix manage the magics)

home.file.".config/waybar/modus.css".source = config.lib.file.mkOutOfStoreSymlink "/home/i/d-git/d-nix/gdk/i-home/configs/waybar/modus.css";

# We will tangle config files from git repo to home dir (Let nix manage the magics)

home.file.".config/ytfzf/conf.sh".source = config.lib.file.mkOutOfStoreSymlink "/home/i/d-git/d-nix/gdk/i-home/configs/ytfzf/conf.sh";

}
