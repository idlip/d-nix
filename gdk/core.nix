{ config, lib, pkgs, inputs, modulesPath, vars, ...}:
{
  # WARNING : This file was generated by d-setup.org
  # DO NOT MODIFY THIS FILE!
  # Any changes made here will be overwritten.

  imports = [

{
  boot = {
    # Uses bleeding edge latest kernel.
    kernelPackages = pkgs.linuxPackages_latest;
    # kernelModules = [ "tcp_bbr" "acpi_call" ];

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
    binfmt.emulatedSystems = ["aarch64-linux"];
  };
}

# Do not modify this file!  It was generated by ‘nixos-generate-config’
# and may be overwritten by future invocations.  Please make changes
# to /etc/nixos/configuration.nix instead.
{
  imports =
    [ (modulesPath + "/installer/scan/not-detected.nix")
    ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "thunderbolt" "vmd" "nvme" "usb_storage" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.kernelParams = [ "i915.force_probe=a7a0"  ];
# "intel_pstate=disable"

  boot.extraModulePackages = [ ];

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/af0bbf7a-ca6f-4823-b548-cc25c63c3d5e";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/68E2-CF64";
      fsType = "vfat";
    };

  swapDevices = [ ];

  # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
  # (the default) this is the recommended approach. When using systemd-networkd it's
  # still possible to use this option, but it's recommended to use it in conjunction
  # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
  networking.useDHCP = lib.mkDefault true;
  # networking.interfaces.wlp0s20f3.useDHCP = lib.mkDefault true;

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}

{
  i18n = {
    defaultLocale = "en_US.UTF-8";
    # saves space
    supportedLocales = [
      "en_US.UTF-8/UTF-8"
      "ja_JP.UTF-8/UTF-8"
      "ro_RO.UTF-8/UTF-8"
    ];
  };

  # Sets big font for bootloader, as I have small laptop.
  # You can remove font and packages line to have default font kernel chooses.
  console = {
    earlySetup = true;
    font = "${pkgs.terminus_font}/share/consolefonts/ter-132n.psf.gz";
    packages = with pkgs; [ terminus_font ];
    keyMap = "us";
  };
}

{
  # obviously your timezone here. Have a nice day or good night sleep ;)
  # Don't waste more time on nixos lol, be healthy and have some sleep. Stay healthy!
  time = {
    timeZone = "Asia/Kolkata";
    hardwareClockInLocalTime = true;
  };
}

{
  users.users.${vars.username} = {
    isNormalUser = true;
    shell = pkgs.zsh;
    extraGroups = ["adbusers" "input" "libvirtd" "networkmanager" "plugdev" "transmission" "video" "wheel"];
  };
}

{
  # compresses half the ram for use as swap
  zramSwap = {
    enable = true;
    memoryPercent = 50;
    algorithm = "zstd";
  };
}

{
  networking = {

    # Killer feature, Its a must these days.
    # Adblocker!! It uses steven black hosts.
    stevenBlackHosts = {
      enable = true;
      blockFakenews = true;
      blockGambling = true;
      blockPorn = true;
      blockSocial = false;
    };

    # dns
    # mullvad dns
    nameservers = [ "194.242.2.5" "194.242.2.9" ];
    dhcpcd = {
      wait = "background";
      extraConfig = "noarp";
    };

    # NetworkManager replaces wpa_supplicant
    wireless.enable = false;

    networkmanager = {
      enable = true;
      unmanaged = ["docker0" "rndis0" "interface-name:ve-*" ];
      wifi.macAddress = "random";
      dns = "systemd-resolved";
      wifi.powersave = true;
    };

    # Firewall uses iptables underthehood
    # Rules are for syncthing
    firewall = rec {
      enable = true;
      # For syncthing & kdeconnect
      allowedTCPPortRanges = [ { from = 1714; to = 1764; } ];
      allowedUDPPortRanges = allowedTCPPortRanges;
      allowedTCPPorts = [8384 22000];
      allowedUDPPorts = [22000 21027];
      allowPing = false;
      logReversePathDrops = true;
    };

    nat = { # for container or vm
      enable = false;
      internalInterfaces = ["ve-+"];
      externalInterface = "wlp0s20f3";
      # Lazy IPv6 connectivity for the container
      enableIPv6 = true;
    };

  };
}

{
  services = {
    # network discovery, mDNS
    avahi = {
      enable = true;
      nssmdns = true;
      publish = {
        enable = true;
        domain = true;
        userServices = true;
      };
    };

    openssh = {
      enable = true;
      settings.UseDns = true;
    };

    # DNS resolver
    resolved = {
      enable = true;
      dnssec = "false";
      fallbackDns = [ "194.242.2.5" "194.242.2.9" ];
    };

    dnscrypt-proxy2 = {
      enable = true;
      settings = {
        ipv6_servers = true;
        require_dnssec = true;

        sources.public-resolvers = {
          urls = [
            "https://raw.githubusercontent.com/DNSCrypt/dnscrypt-resolvers/master/v3/public-resolvers.md"
            "https://download.dnscrypt.info/resolvers-list/v3/public-resolvers.md"
          ];
          cache_file = "/var/lib/dnscrypt-proxy2/public-resolvers.md";
          minisign_key = "RWQf6LRCGA9i53mlYecO4IzT51TGPpvWucNSCh1CBM0QTaLn73Y7GFO3";
        };
      };
    };

  };

}

{
  # Don't wait for network startup
  systemd = {
    services = {
      # speed up boot
      NetworkManager-wait-online.enable = false;
    };
  };
}

{
  # this makes our system more secure
  # note that it might break some stuff, eg webcam

  security = {
    protectKernelImage = true;
    lockKernelModules = false;
    rtkit.enable = true;
    polkit.enable = true;

    # required for lockscreens
    # also for finger print
    pam = {
      services.gtklock = {
        text = "auth include login";
      };
    };

    doas = {
      enable = true;
      extraRules = [{
        users = [ "${vars.username}" ];
        keepEnv = true;
        persist = true;
      }];
    };

    sudo.enable = false;
  };
}

{
  services = {

    dbus = {
      packages = with pkgs; [dconf gcr udisks2];
      enable = true;
    };

    # for minimal journal logs
    journald.extraConfig = ''
      SystemMaxUse=50M
      RuntimeMaxUse=10M
    '';

    # This makes the user to autologin in all tty
    # Depends on you if you want login manager or prefer entering password manually
    # getty.autologinUser = "${vars.username}";

    atd.enable = true; # reminder tool, like @ 2:30 exec this
    fstrim.enable = true; # file system trim
    upower.enable = true; # power utility
  };
}

{
  # For android file transfer via usb, or better could use KDE connect
  services.gvfs.enable = true;
}

{
  # For Laptop, make lid close and power buttom click to suspend
  services.logind = {
    lidSwitch = "suspend-then-hibernate";
    lidSwitchExternalPower = "lock";
    extraConfig = ''
        HandlePowerKey=suspend-then-hibernate
        HibernateDelaySec=3600
      '';
  };
}

{
  # for fingerprint
  services.fprintd = {
    enable = true;
    package = pkgs.fprintd;
  };

}

{
  # for intel cpu to control temp
  services.thermald.enable = true;
}

{
  powerManagement = {
    enable = true;
    powertop.enable = true;
  };
}

{
  services = {
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
  };
}

{
  services = {
    # To mount drives with `udiskctl` command
    udisks2.enable = true;
    printing.enable = true;
  };
}

{
  services.syncthing = {
    enable = true;
    user = "${vars.username}";
    configDir = "/home/${vars.username}/.config/syncthing";
    dataDir = "/home/${vars.username}/.local/share/syncthing";
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
}

{
  # enable zsh autocompletion for system packages (systemd, etc)
  environment = {
    pathsToLink = ["/share/zsh"];
    variables = {
      EDITOR = "emacsclient -nw -a nvim";
      BROWSER = "d-stuff";
      NIXOS_OZONE_WL = "1";
    };
    systemPackages = with pkgs; [
      gitFull
      (writeScriptBin "sudo" ''exec doas "$@"'')
    ];
  };
}

{
  xdg.portal = {
    enable = true;
    extraPortals = [ pkgs.xdg-desktop-portal-gtk pkgs.xdg-desktop-portal-hyprland ];
    configPackages = [ pkgs.xdg-desktop-portal-hyprland ]; # needed from 23.11
  };
}

{
  environment = {
    # set channels (backwards compatibility)
    etc = {
      "nix/flake-channels/nixpkgs".source = inputs.nixpkgs;
      "nix/flake-channels/home-manager".source = inputs.home-manager;
    };
  };
}

{
  nixpkgs = {
    config = {
      allowUnfree = false;
      allowBroken = false;
    };
  };
}

{
  # Collect garbage and delete generation every 7 day. Will help to get some storage space.
  # Better to atleast keep it for few days, as you do major update (unstable), if something breaks you can roll back.
  nix = {
    optimise.automatic = true;
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 7d";
    };

    # Make builds run with low priority so my system stays responsive
    daemonCPUSchedPolicy = "idle";
    daemonIOSchedClass = "idle";

    # pin the registry to avoid downloading and evaling a new nixpkgs version every time
    registry = lib.mapAttrs (_: value: { flake = value; }) inputs;

    # This will additionally add your inputs to the system's legacy channels
    # Making legacy nix commands consistent as well, awesome!
    nixPath = lib.mapAttrsToList (key: value: "${key}=${value.to.path}") config.nix.registry;

    settings = {
      flake-registry = "/etc/nix/registry.json";
      auto-optimise-store = true;
      builders-use-substitutes = true;
      # allow sudo users to mark the following values as trusted
      allowed-users = ["@wheel"];
      # only allow sudo users to manage the nix store
      trusted-users = ["@wheel" "root"];
      keep-outputs = true;
      warn-dirty = false;
      keep-derivations = true;
      sandbox = true;
      max-jobs = "auto";
      # continue building derivations if one fails
      keep-going = true;
      log-lines = 20;
      extra-experimental-features = ["flakes" "nix-command" ];

      # use binary cache, its not gentoo
      substituters = [
        "https://nix-community.cachix.org"
      ];

      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      ];
    };
  };
}

{
  system.autoUpgrade.enable = false;
}

{
  # TODOTHIS: Got some error on docbook,
  # see-> https://github.com/NixOS/nixpkgs/blob/nixos-unstable/nixos/modules/misc/documentation.nix

  # faster rebuilding
  documentation = {
    enable = true;
    nixos.enable = true;
    doc.enable = true;
    info.enable = true;
    man = {
      enable = true;
      generateCaches = true; # will take little time
    };
    dev.enable = true;
  };
}

{
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
}

{
  sound.enable = true;

  # Pipewire setup, just these lines enough to make sane default for it
  services.pipewire = {
    enable = true;
    alsa = {
      enable = true;
      support32Bit = true;
    };
    wireplumber.enable = true;
    pulse.enable = true;
    jack.enable = true;
  };

}

{
  fonts = {
    packages = with pkgs; [
      noto-fonts unifont
      # symbola # this font is unfree
      noto-fonts-emoji maple-mono julia-mono
      (callPackage ./pkgs/code-d-font.nix {})

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

}

{
  # enable programs
  programs = {

    less.enable = true;
    # type "fuck" to fix the last command that made you go "fuck"
    thefuck.enable = true;

    # help manage android devices via command line
    adb.enable = true;

    # ssh.startAgent = true;
    dconf.enable = true;
    noisetorch.enable = true; # virtual noise suppressor

    zsh = {
      enable = true;
      autosuggestions.enable = true;
      syntaxHighlighting = {
        enable = true;
        patterns = {"rm -rf *" = "fg=black,bg=red";};
        styles = {"alias" = "fg=magenta";};
        highlighters = ["main" "brackets" "pattern"];
      };
    };
  };

}

];

  # stateVersion
  # NEVER EVER TOUCH THIS ⚠️
  system.stateVersion = lib.mkDefault "23.05"; # refer manual !

  # configuration ends here
}
