{
  lib,
  pkgs,
  config,
  ...
}: {

  home.packages = with pkgs; [
    jaq
  ];


  # the thing is you won't get xdg-portal-hyprland
  # refer: https://github.com/nix-community/home-manager/issues/1167
  # # enable hyprland
  wayland.windowManager.hyprland = {
    enable = true;
    systemd.enable = true;
    extraConfig = ''
    source					= ~/.config/hypr/main.conf
    '';
    settings = {

      decoration = {
        rounding = 16;
        multisample_edges = true;
        inactive_opacity = 0.8;
        active_opacity = 0.9;
        fullscreen_opacity = 1.0;
        dim_inactive = false;
        shadow_offset = "0 5";
        "col.shadow" = "rgba(00000099)";

        blur = {
          enabled = 1;
          size = 6;
          passes = 3;
          new_optimizations = true;
          ignore_opacity = false;
        };
      };

      "$mod" = "SUPER";

      input = {
        kb_layout = "us";
        kb_options = "ctrl:nocaps";
        follow_mouse = 0;
        sensitivity = 0.1;
        #    repeat_delay = 250

        touchpad = {
          natural_scroll = "no";
          disable_while_typing = 1;
          clickfinger_behavior = 0; # double tap > right click
          middle_button_emulation = 1;
          tap-to-click = 1;
        };
      };

      "device:at-translated-set-2-keyboard" = {
        enabled=true;
      };

      general  =  {
        gaps_in = 5;
        gaps_out = 15;
        border_size = 2;
        "col.active_border"  =  "rgba(e5b9c6ff) rgba(c293a3ff) 45deg";
        "col.inactive_border"  =  "0xff382D2E";
        no_border_on_floating  =  false; # enable border on float window
        layout  =  "dwindle";
        no_cursor_warps  =  false;
      };

      misc = {
        disable_hyprland_logo = true;
        disable_splash_rendering = true;
        mouse_move_enables_dpms = true;
        enable_swallow = true;
        swallow_regex = "^(foot)$";
      };

      animations = {
        enabled = true;
      };

      dwindle = {
        pseudotile = true;
        preserve_split = true;
        force_split = true;
        no_gaps_when_only = false;
        default_split_ratio = 1.0;
        smart_split = false;
      };

      master = {
        new_is_master = false;
        new_on_top = false;
        allow_small_split = true;
        no_gaps_when_only = false;
      };

      gestures  =  {
        workspace_swipe = 1;
        workspace_swipe_distance = 400;
        workspace_swipe_invert = 1;
        workspace_swipe_min_speed_to_force = 30;
        workspace_swipe_cancel_ratio = 0.5;
      };

      bindm = [
        # mouse movements
        "$mod, mouse:272, movewindow"
        "$mod, mouse:273, resizewindow"
        "$mod ALT, mouse:272, resizewindow"
      ];


    };
  };

  xdg.configFile."hypr/main.conf".source = config.lib.file.mkOutOfStoreSymlink "/home/idlip/d-git/d-nix/home/wayland/hyprland.conf";



}
