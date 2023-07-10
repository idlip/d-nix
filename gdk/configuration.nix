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
  hyprland.enable = true;
};

# compresses half the ram for use as swap
zramSwap = {
  enable = true;
  memoryPercent = 50;
  algorithm = "zstd";
};

# obviously your timezone here. Have a nice day or good night sleep ;)
# Don't waste more time on nixos lol, be healthy and have some sleep. Stay helathy!
time.timeZone = "Asia/Kolkata";

# This code is from nixos wiki for Btrfs. Depends on which file system you use.
# Refer nixos wiki once. Might get error if not using btrfs
# #FIXME 
fileSystems = {
  "/".options = [ "compress=zstd" ];
  "/home".options = [ "compress=zstd" ];
  "/nix".options = [ "compress=zstd" "noatime" ];
};  

# Select internationalisation properties.
i18n.defaultLocale = "en_US.UTF-8";
# Sets big font for bootloader, as I have small laptop.
# You can remove font and packages line to have default font kernel chooses.
console = {
  earlySetup = true;
  # font = "${pkgs.unifont}/share/fonts/unifont.pcf.gz";
  # packages = with pkgs; [ unifont ];
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
    # keep a check and remove it asap
    permittedInsecurePackages = [
      "openssl-1.1.1u"
    ];
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
  registry = lib.mapAttrs (_: value: { flake = value; }) inputs;  

  # This will additionally add your inputs to the system's legacy channels  
  # Making legacy nix commands consistent as well, awesome!  
  nixPath = lib.mapAttrsToList (key: value: "${key}=${value.to.path}") config.nix.registry;  

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
  # NOTE : You should do a simple rebuild with these substituters line first,
  # and then install packages from there, as a rebuild will register these cachix into /etc/nix/nix.conf file.
  # If you continue without a rebuild, Emacs will start compiling.
  # So rebuild and make sure you see these substituters in /etc/nix/nix.conf and then add packages.
  settings = {
    auto-optimise-store = true;
    builders-use-substitutes = true;
    trusted-users = ["root" "@wheel"];
    max-jobs = "auto";
    # use binary cache, its not gentoo
    substituters = [
      "https://cache.nixos.org"
      # "https://nix-community.cachix.org"
    ];
    # Keys for the sustituters cachix
    trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      # "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
    ];
  };
};
system.autoUpgrade.enable = false;
system.stateVersion = "23.05"; # DONT TOUCH THIS (See about state version on nixos manual)

boot = {
  # Uses bleeding edge latest kernel.
  kernelPackages = pkgs.linuxPackages_latest;
  kernelModules = ["tcp_bbr"];

  kernel.sysctl = {
    # The Magic SysRq key is a key combo that allows users connected to the
    # system console of a Linux kernel to perform some low-level commands.
    # Disable it, since we don't need it, and is a potential security concern.
    "kernel.sysrq" = 0;

    ## TCP hardening
    # Prevent bogus ICMP errors from filling up logs.
    "net.ipv4.icmp_ignore_bogus_error_responses" = 1;
    # Reverse path filtering causes the kernel to do source validation of
    # packets received from all interfaces. This can mitigate IP spoofing.
    "net.ipv4.conf.default.rp_filter" = 1;
    "net.ipv4.conf.all.rp_filter" = 1;
    # Do not accept IP source route packets (we're not a router)
    "net.ipv4.conf.all.accept_source_route" = 0;
    "net.ipv6.conf.all.accept_source_route" = 0;
    # Don't send ICMP redirects (again, we're on a router)
    "net.ipv4.conf.all.send_redirects" = 0;
    "net.ipv4.conf.default.send_redirects" = 0;
    # Refuse ICMP redirects (MITM mitigations)
    "net.ipv4.conf.all.accept_redirects" = 0;
    "net.ipv4.conf.default.accept_redirects" = 0;
    "net.ipv4.conf.all.secure_redirects" = 0;
    "net.ipv4.conf.default.secure_redirects" = 0;
    "net.ipv6.conf.all.accept_redirects" = 0;
    "net.ipv6.conf.default.accept_redirects" = 0;
    # Protects against SYN flood attacks
    "net.ipv4.tcp_syncookies" = 1;
    # Incomplete protection again TIME-WAIT assassination
    "net.ipv4.tcp_rfc1337" = 1;

    ## TCP optimization
    # TCP Fast Open is a TCP extension that reduces network latency by packing
    # data in the sender’s initial TCP SYN. Setting 3 = enable TCP Fast Open for
    # both incoming and outgoing connections:
    "net.ipv4.tcp_fastopen" = 3;
    # Bufferbloat mitigations + slight improvement in throughput & latency
    "net.ipv4.tcp_congestion_control" = "bbr";
    "net.core.default_qdisc" = "cake";
  };

  loader = {
    # FIXME change first line if you want to use Grub
    systemd-boot.enable = true;
    efi.canTouchEfiVariables = true;
    timeout = 1;
  };

  supportedFilesystems = [ "ntfs" ];
  tmp.cleanOnBoot = true;
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
  # gnome.at-spi2-core.enable = true;

  # tlp.enable = true;     # TLP and auto-cpufreq for power management
  auto-cpufreq.enable = true;

  # For Laptop, make lid close and power buttom click to suspend
  logind = {
    lidSwitch = "suspend";
    extraConfig = ''
  HandlePowerKey = suspend
  '';
  };

  atd.enable = true;
  fstrim.enable = true;
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
      "realme" = { id = "CEV3U3M-EJFLUJ3-UXFBEPG-KHX5EVK-3MSYH2W-BRNZEDH-TVJ4QWZ-X3G2CAW"; };
      #"device2" = { id = "DEVICE-ID-GOES-HERE"; };
    };
    folders = {
      "sync" = {
	path = "/home/i/d-sync";
	devices = [ "realme" ];
      };
      "emacs" = {
	path = "/home/i/d-git/d-nix";
	devices = [ "realme" ];
      };
      "theme" = {
	path = "/home/i/d-git/d-theme";
	devices = [ "realme" ];
      };
      "site" = {
	path = "/home/i/d-git/d-site";
	devices = [ "realme" ];
      };
    };
  };

  # This makes the user 'i' to autologin in all tty
  # Depends on you if you want login manager or prefer entering password manually
  getty.autologinUser = "i" ;

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
  # For wayland users
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
  gitFull
  neovim helix
];

# Add other overlays here
# nixpkgs.overlays = with inputs; [emacs-overlay.overlay];

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
    noto-fonts unifont
    symbola noto-fonts-emoji maple-mono
    (callPackage ./pkgs/code-d-font.nix {})

    (nerdfonts.override {fonts = [ "JetBrainsMono"  ];})
  ];

  enableDefaultFonts = false;

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

environment = {
  variables = {
    NIXOS_OZONE_WL = "1";
    EDITOR = "nvim";
    BROWSER = "firefox";
    MOZ_ENABLE_WAYLAND = "1";
  };
};

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
