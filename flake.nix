#
# This file is auto-generated from "README.org"
#

{
  description = "Nix Organized with Emacs";

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

  outputs = {nixpkgs, ...}@inputs:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.x86_64-linux;

      vars = {
        username = "idlip";
        editor = "emacs";
      };

    in {

      devShells.x86_64-linux.default = pkgs.mkShell {
        packages = with pkgs; [
          alejandra deadnix git
        ];
        name = "dots";
        DIRENV_LOG_FORMAT = "";
      };

      nixosConfigurations = {
        gdk = nixpkgs.lib.nixosSystem {
          system = "x86_64-linux";
          modules =
            [
              ./gdk/core.nix
              inputs.hosts.nixosModule
              inputs.home-manager.nixosModules.home-manager
              {
                home-manager = {
                  useUserPackages = true;
                  useGlobalPkgs = true;
                  extraSpecialArgs = {
                    inherit inputs vars;
                  };
                  users.${vars.username} = import ./gdk/home.nix;
                };
              }
            ];
          specialArgs = {
            inherit inputs;
            inherit vars system pkgs;
          };
        };
      };
    };
}
