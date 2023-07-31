{
  self,
  nixpkgs,
  ...
}: let
  inherit (self) inputs;
  bootloader = ../modules/bootloader.nix;
  core = ../modules;
  hmModule = inputs.home-manager.nixosModules.home-manager;
  blockhost = inputs.hosts.nixosModule;

  shared = [ core ];

  home-manager = {
    useUserPackages = true;
    useGlobalPkgs = true;
    extraSpecialArgs = {
      inherit inputs;
      inherit self;
    };
    users.idlip = ../home;
  };

in {

  # laptop
  gdk = nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    modules =
      [
        {networking.hostName = "gdk";}
        ./gdk/hardware-configuration.nix
        bootloader
        hmModule
        {inherit home-manager;}
        blockhost
      ]
      ++ shared;
    specialArgs = {inherit inputs;};
  };
}
