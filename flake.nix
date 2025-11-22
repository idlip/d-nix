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

	quickshell = {
      url = "github:outfoxxed/quickshell";
      inputs.nixpkgs.follows = "nixpkgs";
    };

	noctalia = {
      url = "github:noctalia-dev/noctalia-shell";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.quickshell.follows = "quickshell";
    };

    vicinae = {
	  url = "github:vicinaehq/vicinae";
	};

	youtube_adblock = {
	  url = "https://raw.githubusercontent.com/afreakk/greasemonkeyscripts/refs/heads/master/youtube_adblock.js";
	  flake = false;
	};
	reddit_adblock = {
	  url = "https://raw.githubusercontent.com/afreakk/greasemonkeyscripts/refs/heads/master/reddit_adblock.js";
	  flake = false;
	};
	youtube_sponsorblock = {
	  url = "https://raw.githubusercontent.com/afreakk/greasemonkeyscripts/refs/heads/master/youtube_sponsorblock.js";
	  flake = false;
	};
	html_player = {
	  url = "https://update.greasyfork.org/scripts/30545/HTML5%E8%A7%86%E9%A2%91%E6%92%AD%E6%94%BE%E5%B7%A5%E5%85%B7.user.js";
	  flake = false;
	};
	adsbypasser = {
	  url = "https://adsbypasser.github.io/releases/adsbypasser.full.es7.user.js";
	  flake = false;
	};
	torrent_1337 = {
	  url = "https://update.greasyfork.org/scripts/33379/1337x%20-%20Torrent%20page%20improvements.user.js";
	  flake = false;
	};
	bypass_all = {
	  url = "https://codeberg.org/Amm0ni4/bypass-all-shortlinks-debloated/raw/branch/main/Bypass_All_Shortlinks.user.js";
	  flake = false;
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
  };

  outputs =
    { nixpkgs, home-manager, nix-index-database, stylix, noctalia, quickshell, ... }@inputs:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.x86_64-linux;

	  hostVars = {
        gdk = {
		  host = "gdk";
          username = "idlip";
          editor = "emacs";
        };
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
          ${hostVars.gdk.host} = nixpkgs.lib.nixosSystem {
            system = system;
            modules = [
              ./${hostVars.gdk.host}/core.nix
              inputs.home-manager.nixosModules.home-manager
              nix-index-database.nixosModules.nix-index
		      stylix.nixosModules.stylix
              ];
            specialArgs = {
			  inherit inputs system;
              vars = hostVars.gdk;
            };
          };

		  ${hostVars.foss.host} = nixpkgs.lib.nixosSystem {
			system = system;
			modules = [
              ./${hostVars.foss.host}/core.nix
              inputs.home-manager.nixosModules.home-manager
              nix-index-database.nixosModules.nix-index
              stylix.nixosModules.stylix
			];
			specialArgs = {
              inherit inputs system;
              vars = hostVars.foss;
			};
          };
        };
      };
}
