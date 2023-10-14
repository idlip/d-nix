{pkgs, lib, config, ...}: let
in {
  # screen idle
  services.swayidle = {
    enable = true;
    events = [
      {
        event = "before-sleep";
        command = "${pkgs.systemd}/bin/loginctl lock-session";
      }
      {
        event = "lock";
        command = "${pkgs.gtklock}/bin/gtklock";
      }
    ];
    timeouts = [
      {
        timeout = 250;
        command = "gtklock & sleep 1 && systemctl suspend";
        # command = "${config.wayland.windowManager.hyprland.package}/bin/hyprctl dispatch dpms off";
        # resumeCommand = "${config.wayland.windowManager.hyprland.package}/bin/hyprctl dispatch dpms on";
      }
    ];
  };

  systemd.user.services.swayidle.Install.WantedBy = lib.mkForce ["hyprland-session.target"];

}
