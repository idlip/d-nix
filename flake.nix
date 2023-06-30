{
  description = "D Nixyfied ❄️ Flaky Station";

  inputs = {

    # Change it to stable, if you want stable channel (23.05)
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    # For Adblocking and making internet usable
    hosts.url = "github:StevenBlack/hosts";

    # Greatest Editor of all time. (if you shill on vim, give emacs a try and see first, although it is objective to individual)
    # emacs-overlay = {   
    #   url = "github:nix-community/emacs-overlay";
    # };

    # Home to manage all user configs
    # You can you use normal config files,
    # just use an org heading and create block and tangle it directly to ~/.config/tool/file path.
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

  };

  outputs = inputs @ {self, hosts, home-manager, nixpkgs, ...} :
    let
      # You might check on darwin for macos
      system = "x86_64-linux";
      pkgs = import nixpkgs {
	inherit system;
	config.allowUnfree = true;
      };
      lib = nixpkgs.lib;

    in {

      nixosConfigurations = {

	gdk = lib.nixosSystem {
			  inherit system;
			  modules = [

			    ./gdk/configuration.nix
			      home-manager.nixosModules.home-manager {
				home-manager.useGlobalPkgs = true;
				home-manager.useUserPackages = true;
				home-manager.extraSpecialArgs = {
				  inherit inputs;
				  inherit self;
				};

				home-manager.users.i = ./gdk/i-home ;
			      }
			      hosts.nixosModule
			  ];
			  specialArgs = {inherit inputs;};
			};
      };

      packages.${system} = {
	code-d-font = pkgs.callPackage ./gdk/pkgs/code-d-font.nix {};
      };
    };
}
