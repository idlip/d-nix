#
# This file is auto-generated from "d-setup.org"
#
{
  description = "Nix Organized with Emacs";

  inputs = {
    # Change it to stable, if you want stable channel (26.05)
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    # Home to manage all user configs
    # You can you use normal config files,
    # just use an org heading and create block and tangle it directly to ~/.config/tool/file path.
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # nix index to locate package/path
    nix-index-database = {
      url = "github:nix-community/nix-index-database";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
    };

    stylix = {
      url = "github:nix-community/stylix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    mango = {
      url = "github:DreamMaoMao/mango";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    glide = {
      url = "github:glide-browser/glide.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    dns-block = {
      url = "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/hosts/ultimate-compressed.txt";
      flake = false;
    };
    dns-tif = {
      url = "https://raw.githubusercontent.com/hagezi/dns-blocklists/main/hosts/tif-compressed.txt";
      flake = false;
    };
    dns-bpc = {
      url = "https://gitflic.ru/project/magnolia1234/bypass-paywalls-clean-filters/blob/raw?file=bpc-paywall-filter.txt";
      flake = false;
    };
    ewm = {
      url = "https://codeberg.org/ezemtsov/ewm/archive/feat/workspace-animations.tar.gz";
    };
  };

  outputs =
    { nixpkgs, home-manager, nix-index-database, stylix, ... }@inputs:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.x86_64-linux;

	  hostVars = {
        foss = {
		  host = "foss";
          username = "dev";
          editor = "emacs";
        };
      };
    in
      {
        devShells.x86_64-linux.default = pkgs.mkShell {
          packages = with pkgs; [
            statix nixfmt-rfc-style
            # nixd
          ];
          name = "dots";
          DIRENV_LOG_FORMAT = "";
        };

        nixosConfigurations = {
		      ${hostVars.foss.host} = nixpkgs.lib.nixosSystem {
			      system = system;
			      modules = [
              ./${hostVars.foss.host}/core.nix
              inputs.home-manager.nixosModules.home-manager
              nix-index-database.nixosModules.nix-index
              stylix.nixosModules.stylix
              inputs.mango.nixosModules.mango
              inputs.ewm.nixosModules.default
			      ];
			      specialArgs = {
              inherit inputs system;
              vars = hostVars.foss;
			      };
          };
        };
      };
}
