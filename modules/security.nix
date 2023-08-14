{
  config,
  pkgs,
  lib,
  username,
  ...
}:

# this makes our system more secure
# note that it might break some stuff, eg webcam

{
  services = {
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

  programs.ssh.startAgent = true;
  programs.firejail = let
    profiles = "${lib.getExe pkgs.firejail}/etc/firejail";
    inherit (lib) getExe;
  in {
    enable = true;
    wrappedBinaries = with pkgs; {
      brave = {
        executable = getExe pkgs.brave;
        profile = "${profiles}/brave-browser-stable.profile";
      };
      keepassxc = {
        executable = getExe keepassxc;
        profile = "${profiles}/keepassxc.profile";
      };
      sioyek = {
        executable = getExe pkgs.sioyek;
        profile = "${profiles}/sioyek.profile";
      };
      tor = {
        executable = getExe pkgs.tor;
        profile = "${profiles}/tor.profile";
      };
      aria2c = {
        executable = getExe pkgs.aria2;
        profile = "${profiles}/aria2c.profile";
      };
    };
  };

  security = {
    protectKernelImage = true;
    lockKernelModules = false;
    rtkit.enable = true;
    polkit.enable = true;

    doas = {
      enable = true;
      extraRules = [{
        users = [ username ];
        keepEnv = true;
        persist = true;
      }];
    };

    sudo.enable = false;
  };

}
