{lib, ...}:
# personal lib
# inspired by mihai (@fufexan)

let

  colorlib = import ./colors.nix lib;

in {
  imports = [
    {
      # get default across the module system
      _module.args = {
        default = import ./theme {inherit colorlib lib;};
      };
    }
  ];
}
