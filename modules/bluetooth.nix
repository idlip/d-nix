{pkgs, ...}: {

  hardware.bluetooth = {
    enable = true;
    package = pkgs.bluez5-experimental;
    settings = {
      General = {
        Class = "0x000100";
        ControllerMode = "bredr";
        FastConnectable = true;
        JustWorksRepairing = "always";
        Privacy = "device";
        Experimental = true;
      };
    };
  };

  # https://github.com/NixOS/nixpkgs/issues/114222
  systemd.user.services.telephony_client.enable = false;
}
