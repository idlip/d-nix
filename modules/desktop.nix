{
  config,
  pkgs,
  lib,
  ...
}: {

  programs = {
    dconf.enable = true;
    hyprland.enable = true;
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

    # tlp.enable = true;     # TLP and auto-cpufreq for power management
    auto-cpufreq.enable = true;

    # For Laptop, make lid close and power buttom click to suspend
    logind = {
      lidSwitch = "suspend";
      extraConfig = ''
        HandlePowerKey = suspend
      '';
    };

    # This makes the user 'idlip' to autologin in all tty
    # Depends on you if you want login manager or prefer entering password manually
    getty.autologinUser = "idlip";

    atd.enable = true;
    fstrim.enable = true;
    upower.enable = true;

    # For android file transfer via usb, or better check on KDE connect
    gvfs.enable = true;

    # configuring syncthing
    syncthing = {
      enable = true;
      user = "idlip";
      configDir = "/home/idlip/.config/syncthing";
      overrideDevices = true;     # overrides any devices added or deleted through the WebUI
      overrideFolders = true;     # overrides any folders added or deleted through the WebUI
      settings = {
        devices = {
          "realme" = { id = "CEV3U3M-EJFLUJ3-UXFBEPG-KHX5EVK-3MSYH2W-BRNZEDH-TVJ4QWZ-X3G2CAW"; };
          #"device2" = { id = "DEVICE-ID-GOES-HERE"; };
        };
        folders = {
          "sync" = {
	          path = "~/d-sync";
	          devices = [ "realme" ];
          };
          "emacs" = {
	          path = "~/d-git/d-nix";
	          devices = [ "realme" ];
          };
          "theme" = {
	          path = "~/d-git/d-theme";
	          devices = [ "realme" ];
          };
          "site" = {
	          path = "~/d-git/d-site";
	          devices = [ "realme" ];
          };
        };
      };
    };

    # Pipewire setup, just these lines enought to make sane default for it
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
    opengl = {
      enable = true;
      # extraPackages = with pkgs; [
      #   vaapiVdpau
      #   libvdpau-va-gl
      #   # intel-media-driver
      #   # vaapiIntel
      # ];
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
        serif = [ "Code D Ace" "Noto Sans" "Noto Serif"];
        emoji = [ "Noto Color Emoji" "Code D Lip" "Symbola" "Noto Sans" ];
      };
    };
  };

  xdg.portal = {
    enable = true;
    extraPortals = with pkgs; [ xdg-desktop-portal-gtk xdg-desktop-portal-hyprland ];
  };

}
