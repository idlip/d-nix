{
  config,
  pkgs,
  lib,
  ...
}: {

  programs = {
    dconf.enable = true;
    noisetorch.enable = true; # virtual noise suppressor
  };

  powerManagement = {
    enable = true;
    powertop.enable = true;
  };

  services = {

    dbus = {
      packages = with pkgs; [dconf gcr udisks2];
      enable = true;
    };
    udev.packages = with pkgs; [ android-udev-rules ];

    journald.extraConfig = ''
      SystemMaxUse=50M
      RuntimeMaxUse=10M
    '';

    # To mount drives with udiskctl command
    udisks2.enable = true;
    printing.enable = true;

    fprintd = {
      enable = true;
      package = pkgs.fprintd;
    };

    # power management
    # auto-cpufreq= {
    #   enable = true;
    #   settings = {
    #     battery = {
    #       governor = "powersave";
    #       turbo = "never";
    #     };
    #     charger = {
    #       governor = "performance";
    #       turbo = "auto";
    #     };
    #   };
    # };

    thermald.enable = true;
    tlp = {
      enable = true;
      settings = {
        CPU_SCALING_GOVERNOR_ON_AC = "balance_performance";
        CPU_SCALING_GOVERNOR_ON_BAT = "powersave";

        CPU_ENERGY_PERF_POLICY_ON_BAT = "balance_power";
        CPU_ENERGY_PERF_POLICY_ON_AC = "balance_performance";

        USB_AUTOSUSPEND = 1;
        DEVICES_TO_DISABLE_ON_STARTUP = "bluetooth";
        DEVICES_TO_DISABLE_ON_BAT_NOT_IN_USE = "bluetooth";

        CPU_BOOST_ON_AC = 0;
        CPU_BOOST_ON_BAT = 0;
        CPU_HWP_DYN_BOOST_ON_AC = 1;
        CPU_HWP_DYN_BOOST_ON_BAT = 0;

        CPU_MIN_PERF_ON_AC = 0;
        CPU_MAX_PERF_ON_AC = 70;
        CPU_MIN_PERF_ON_BAT = 0;
        CPU_MAX_PERF_ON_BAT = 30;
      };
    };


    # For Laptop, make lid close and power buttom click to suspend
    logind = {
      lidSwitch = "suspend-then-hibernate";
      lidSwitchExternalPower = "lock";
      extraConfig = ''
        HandlePowerKey=suspend-then-hibernate
        HibernateDelaySec=3600
      '';

    };

    # This makes the user 'idlip' to autologin in all tty
    # Depends on you if you want login manager or prefer entering password manually
    # getty.autologinUser = "idlip";

    atd.enable = true;
    fstrim.enable = true;
    upower.enable = true;

    # For android file transfer via usb, or better check on KDE connect
    gvfs.enable = true;

    # Pipewire setup, just these lines enough to make sane default for it
    pipewire = {
      enable = true;
      alsa = {
        enable = true;
        support32Bit = true;
      };
      wireplumber.enable = true;
      pulse.enable = true;
      jack.enable = true;
    };
  };

  sound.enable = true;

  systemd.services = {
    seatd = {
      enable = true;
      description = "Seat management daemon";
      script = "${pkgs.seatd}/bin/seatd -g wheel";
      serviceConfig = {
        Type = "simple";
        Restart = "always";
        RestartSec = "1";
      };
      wantedBy = ["multi-user.target"];
    };
  };


  hardware = {
    pulseaudio.enable = lib.mkForce false;
    opengl = {
      enable = true;
      extraPackages = with pkgs; [
        libva intel-media-driver
        vaapiVdpau vaapiIntel
        libvdpau-va-gl
      ];
    };
  };

  fonts = {
    packages = with pkgs; [
      noto-fonts unifont
      # symbola # this font is unfree
      noto-fonts-emoji maple-mono
      (callPackage ../pkgs/code-d-font.nix {})

      (nerdfonts.override {fonts = [ "JetBrainsMono"  ];})
    ];

    enableDefaultPackages = true;

    # this fixes emoji stuff
    fontconfig = {
      defaultFonts = {
        monospace = [
	        "Code D OnePiece"
	        "JetBrainsMono Nerd Font"
	        "Noto Color Emoji"
        ];
        sansSerif = [ "Code D Haki" "Noto Sans" "Noto Serif" ];
        serif = [ "Code D Haki" "Noto Sans" "Noto Serif"];
        emoji = [ "Noto Color Emoji" "Code D Lip" "Symbola" "Noto Sans" ];
      };
    };
  };

  xdg.portal = {
    enable = true;
    extraPortals = with pkgs; [ xdg-desktop-portal-gtk xdg-desktop-portal-hyprland ];
  };

}
