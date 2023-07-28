{
  description = "D Nixyfied ❄️ Flaky Station";

  inputs = {

    # Change it to stable, if you want stable channel (23.05)
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    # For Adblocking and making internet usable
    hosts.url = "github:StevenBlack/hosts";

    # nix index to locate package/path
    nix-index-db = {
      url = "github:nix-community/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Home to manage all user configs
    # You can you use normal config files,
    # just use an org heading and create block and tangle it directly to ~/.config/tool/file path.
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

  };

  outputs = {self, ...} @ inputs: let
    system = "x86_64-linux";
    pkgs = inputs.nixpkgs.legacyPackages.x86_64-linux;
  in {

    nixosConfigurations = import ./hosts inputs;

  };
}
