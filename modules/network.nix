{
  ...
}: {
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
  };


  # Don't wait for network startup
  systemd = {
    services = {
      # speed up boot
      NetworkManager-wait-online.enable = false;
    };
  };
}
