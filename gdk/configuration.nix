# WARNING : This file was generated by d-setup.org
# DO NOT MODIFY THIS FILE!
# Any changes made here will be overwritten.

{ inputs, lib, self, config, pkgs, ... }:

{

imports = [
  ./hardware-configuration.nix
];

# Should move this line probably, it only does make use of starship to bash shell init
programs = {
  bash.promptInit = ''eval "$(${pkgs.starship}/bin/starship init bash)"'';
};

# compresses half the ram for use as swap
zramSwap = {
  enable = true;
  memoryPercent = 50 ;
  algorithm = "zstd";
};

# obviously your timezone here. Have a nice day or good night sleep ;)
# Don't waste more time on nixos lol, be healthy and have some sleep. Stay helathy!
time.timeZone = "Asia/Kolkata";

# This code is from nixos wiki for Btrfs. Depends on which file system you use.
# Refer nixos wiki once.
fileSystems = {
  "/".options = [ "compress=zstd" ];
  "/home".options = [ "compress=zstd" ];
  "/nix".options = [ "compress=zstd" "noatime" ];
};  

# Select internationalisation properties.
i18n.defaultLocale = "en_US.UTF-8";
# Sets big font for bootloader, as I have small laptop. You can remove font and packages line to have default vanilla font.
console = {
  earlySetup = true;
  font = "${pkgs.terminus_font}/share/consolefonts/ter-132n.psf.gz";
  packages = with pkgs; [ terminus_font ];
  keyMap = "us";
};

environment = {
  # set channels (backwards compatibility)
  etc = {
    "nix/flake-channels/nixpkgs".source = inputs.nixpkgs;
    "nix/flake-channels/home-manager".source = inputs.home-manager;
  };
};

# As name implies, allows Unfree packages. You can enable in case you wanna install non-free tools (eg: some fonts lol)
nixpkgs = {
  config = {
    allowUnfree = true;
    allowBroken = false;
  };

};

# faster rebuilding
documentation = {
  enable = true;
  doc.enable = false;
  man.enable = true;
  dev.enable = false;
};

# Collect garbage and delete generation every 6 day. Will help to get some storage space.
# Better to atleast keep it for few days, as you do major update (unstable), if something breaks you can roll back.
nix = {
  gc = {
    automatic = true;
    dates = "daily";
    options = "--delete-older-than 6d";
  };

  # pin the registry to avoid downloading and evaling a new nixpkgs version every time
  registry = lib.mapAttrs (_: v: {flake = v;}) inputs;

  # set the path for channels compat
  nixPath = [
    "nixpkgs=/etc/nix/flake-channels/nixpkgs"
    "home-manager=/etc/nix/flake-channels/home-manager"
  ];

  # Free up to 1GiB whenever there is less than 100MiB left.
  extraOptions = ''
    experimental-features = nix-command flakes
    keep-outputs = true
    warn-dirty = false
    keep-derivations = true
    min-free = ${toString (100 * 1024 * 1024)}
    max-free = ${toString (1024 * 1024 * 1024)}
  '';

  # substituters are cachix domain, where some package binaries are available (eg : Hyprland & Emacs 30)
  # NOTE : You should do a simple rebuild with these substituters line and then install packages from there, as a rebuild will register these cachix into /etc/nix/nix.conf file. If you continue without rebuild, Hyprland and Emacs will start compiling. So rebuild and make sure you see these substituters in /etc/nix/nix.conf and then add packages.
  settings = {
    auto-optimise-store = true;
    builders-use-substitutes = true;
    trusted-users = ["root" "@wheel"];
    max-jobs = "auto";
    # use binary cache, its not gentoo
    substituters = [
      "https://cache.nixos.org"
      "https://nixpkgs-wayland.cachix.org"
      "https://nix-community.cachix.org"
      "https://hyprland.cachix.org"
    ];
    # Keys for the sustituters cachix
    trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "nixpkgs-wayland.cachix.org-1:3lwxaILxMRkVhehr5StQprHdEo4IrE8sRho9R9HOLYA="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="
    ];
  };
};
system.autoUpgrade.enable = false;
system.stateVersion = "22.11"; # DONT TOUCH THIS (See about state version on nixos manual)

boot = {
  # Uses bleeding edge latest kernel. 
  kernelPackages = pkgs.linuxPackages_latest;

  loader = {
    # FIXME change first line if you want to use Grub
    systemd-boot.enable = true;
    efi.canTouchEfiVariables = true;
    timeout = 5;
  };

  tmp = {
    cleanOnBoot = true;
  };
};

networking = {
  hostName = "gdk";
  # dns
  networkmanager = {
    enable = true;
    unmanaged = ["docker0" "rndis0"];
    wifi.macAddress = "random";
  };

  # Killer feature, Its a must these days.
  # Adblocker!! It uses steven black hosts.
  stevenBlackHosts = {
    enable = true;
    blockFakenews = true;
    blockGambling = true;
    blockPorn = true;
    blockSocial = false;
  };

  # Firewall uses iptables underthehood
  # Rules are for syncthing
  firewall = {
    enable = true;
    # For syncthing
    allowedTCPPorts = [8384 22000];
    allowedUDPPorts = [22000 21027];
    allowPing = false;
    logReversePathDrops = true;
  };
};
# Avoid slow boot time
systemd.services.NetworkManager-wait-online.enable = false;

security.rtkit.enable = true;
security.polkit.enable = true;
security.sudo.enable = false;
# Configure doas
security.doas = {
  enable = true;
  extraRules = [{
    users = [ "i" ];
    keepEnv = true;
    persist = true;  
  }];
};

services = {
  dbus = {
    packages = with pkgs; [dconf udisks2 gcr];
    enable = true;
  };
  # udev.packages = with pkgs; [gnome.gnome-settings-daemon];

  journald.extraConfig = ''
    SystemMaxUse=50M
    RuntimeMaxUse=10M
  '';
  # To mount drives with udiskctl command
  udisks2.enable = true;
  gnome.at-spi2-core.enable = true;

  # tlp.enable = true;     # TLP and auto-cpufreq for power management
  auto-cpufreq.enable = true;

  # For Laptop, make lid close and power buttom click to suspend
  logind = {
    lidSwitch = "suspend";
    extraConfig = ''
  HandlePowerKey = suspend
  '';
  };

  # See if you want bluetooth setup
  # blueman.enable = true;

  # For android file transfer via usb, or better check on KDE connect 
  gvfs.enable = true;

  # configuring syncthing
  syncthing = {
    enable = true;
    user = "i";
    configDir = "/home/i/.config/syncthing";
    overrideDevices = true;     # overrides any devices added or deleted through the WebUI
    overrideFolders = true;     # overrides any folders added or deleted through the WebUI
    devices = {
      "realme" = { id = "5ZNAQ2Z-T2DD757-6JK53J6-4NFMMGG-ETTFU5W-UNAMYLV-XM3P6CZ-ERSRTQX"; };
      #"device2" = { id = "DEVICE-ID-GOES-HERE"; };
    };
    folders = {
      "music-jazz" = {        # Name of folder in Syncthing, also the folder ID
        path = "/home/i/music";    # Which folder to add to Syncthing
        devices = [ "realme" ];      # Which devices to share the folder with
      };
      "syncs" = {
        path = "/home/i/sync";
        devices = [ "realme" ];
        ignorePerms = false; 
      };
      "essentials" = {
        path = "/home/i/d/Essentials";
        devices = [ "realme" ];
      };
    };
  };

  fstrim.enable = true;

  # This makes the user 'i' to autologin in all tty
  # Depends on you if you want login manager or prefer entering password manually

  getty.autologinUser = "i" ;

  atd.enable = true;

  # Pipewire setup, just these lines enought to make sane default for it
  pipewire = {
    enable = true;
    alsa = {
      enable = true;
    };
    wireplumber.enable = true;
    pulse.enable = true;
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

environment.systemPackages = with pkgs; [
  firefox
  gitFull
  ntfs3g
  neovim helix
];

# Add other overlays here
nixpkgs.overlays = with inputs; [emacs-overlay.overlay  ];

hardware = {
  opengl = {
    enable = true;
    extraPackages = with pkgs; [
      vaapiVdpau
      libvdpau-va-gl
      intel-media-driver
      vaapiIntel
    ];
  };
};

fonts = {
  fonts = with pkgs; [
    emacs-all-the-icons-fonts
    noto-fonts
    #material-icons comic-mono material-design-icons
    # weather-icons font-awesome
    symbola noto-fonts-emoji comic-mono maple-mono-NF
    iosevka-comfy.comfy iosevka-comfy.comfy-motion
    (nerdfonts.override {fonts = ["VictorMono" "FiraCode" "JetBrainsMono"];})
  ];

  enableDefaultFonts = false;

  # this fixes emoji stuff
  fontconfig = {
    defaultFonts = {
      monospace = [
        "ComicCodeLigatures Nerd Font"
        "FiraCode Nerd Font"
        "JetBrainsMono Nerd Font"
        "Noto Color Emoji"
      ];
      sansSerif = [ "Gandhi Sans" "VictorMono Nerd Font" "Noto Sans"];
      serif = [ "Gandhi Sans" "Noto Sans" "VictorMono Nerd Font" "JetBrainsMono Nerd Font" "Noto Sans"];
      emoji = ["ComicCodeLigatures Nerd Font" "Noto Color Emoji" "FiraCode Nerd Font" "Symbola" "all-the-icons" "Weather Icons" "Material Icons" ];
    };
  };
};

environment = {

  variables = {
    NIXOS_OZONE_WL = "1";
    EDITOR = "nvim";
    BROWSER = "firefox";
    MOZ_ENABLE_WAYLAND = "1";

  };

};

environment.interactiveShellInit = ''

'';

users.users.i = {
                              isNormalUser = true;
                              extraGroups = [
                                "wheel"
                                "gitea"
                                "docker"
                                "systemd-journal"
                                "audio"
                                "plugdev"
                                "wireshark"
                                "video"
                                "input"
                                "lp"
                                "networkmanager"
                                "power"
                                "nix"
                              ];
                              uid = 1000;
                              # Use fish if you prefer it
                              shell = pkgs.zsh;

                              # Or else login to root (which you will create while rebuilding) and run passwd USERNAME 
                              # initialPassword = "changeme";
                            };
programs.zsh.enable = true;

}
