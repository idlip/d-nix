{
  description = "D Nixyfied ❄️ Flaky Station";

  inputs = {

    # Change it to table, if you want stable channel
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    # Idk where is this used, I just have it
    nixpkgs-wayland.url = "github:nix-community/nixpkgs-wayland";

    # For firefox extension
    nur.url = "github:nix-community/nur";

    # For Adblocking and making internet usable
    hosts.url = "github:StevenBlack/hosts";

    # Oh yeah, the best wayland compositor out there
    hyprland = {  
      url = "github:hyprwm/Hyprland";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    xdg-portal-hyprland.url = "github:hyprwm/xdg-desktop-portal-hyprland";

    # Greatest editor of all time. (if you shill on vim, give emacs a try and see first, it objective to individuals)
    emacs-overlay = {   
      url = "github:nix-community/emacs-overlay";
    };

    # Home to manage all user configs
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

  };

  outputs = inputs @ {self, hyprland, nur, hosts, home-manager, nixpkgs, ...} :
    let
      # You might check on darwin, or its this is enough
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
  };
}
