{
  description = "D Nixyfied ❄️ Flaky Station";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-wayland.url = "github:nix-community/nixpkgs-wayland";
    nur.url = "github:nix-community/nur";
    hosts.url = github:StevenBlack/hosts;
    hyprland = {  
      url = "github:hyprwm/Hyprland";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    xdg-portal-hyprland.url = "github:hyprwm/xdg-desktop-portal-hyprland";

    emacs-overlay = {                                                     # Emacs Overlays
      url = "github:nix-community/emacs-overlay";
    };
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

  };

  outputs = inputs @ {self, hyprland, nur, hosts, home-manager, nixpkgs, ...} :
    let
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
