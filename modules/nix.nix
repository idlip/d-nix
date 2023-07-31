{
  config,
  pkgs,
  lib,
  inputs,
  ...
}: {

  environment = {
    # set channels (backwards compatibility)
    etc = {
      "nix/flake-channels/nixpkgs".source = inputs.nixpkgs;
      "nix/flake-channels/home-manager".source = inputs.home-manager;
    };
  };

  nixpkgs = {
    config = {
      # keep a check and remove it asap
      # permittedInsecurePackages = [
      #   "openssl-1.1.1u"
      # ];
      allowUnfree = false;
      allowBroken = false;
    };
  };

  # TODOTHIS: Got some error on docbook,
  # see-> https://github.com/NixOS/nixpkgs/blob/nixos-unstable/nixos/modules/misc/documentation.nix

  # faster rebuilding
  documentation = {
    enable = true;
    nixos.enable = false;
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
      options = "--delete-older-than 15d";
    };
    package = pkgs.nixUnstable;

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
  system.autoUpgrade.enable = false;
}
