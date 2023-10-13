{pkgs, config, ...}:

{

  programs.waybar = {
    enable = true;

    settings = {
      mainBar = {
        layer = "top";
        position = "top";
        # height = 15;
        # spacing = 7;
        fixed-center = true;
        exclusive = true;

        modules-left = [
	        "custom/launcher"
	        "hyprland/workspaces"
	        "hyprland/window"
	        "hyprland/submap"
        ];

        modules-center = [
	        "custom/wf-recorder" "clock" "mpd"
        ];

        modules-right = [ "tray" "network" "battery" "memory" "pulseaudio" "custom/power" ];

        "hyprland/workspaces" = {
	        format = "{icon}";
	        active-only = false;
          show-special = true;
	        on-click = "activate";
	        format-icons = {
		        active = "";
		        default = "";
		        "1" = "1";
		        "2" = "2";
		        "3" = "3";
		        "4" = "4";
		        "5" = "5";
		        "6" = "6";
	        };
        };

        "hyprland/window" = {
	        "format" = "{}";
	        "separate-outputs" = true;
          "max-length" = 35;
	        "rewrite" = {
		        "(.*) - Mozilla Firefox" = "🦊 $1";
		        "(.*) - LibreWolf" = "🐺 $1";
		        "(.*) - Brave" = "🦁 $1";
		        "(.*) - GNU Emacs (.*)" = " $1";
		        "(.*).epub(.*)" = "󰂽 $1";
		        "(.*)foot" = " Terminal $1";
	        };
        };

        "hyprland/submap" = {
	        "format" = " {}";
	        "max-length" = 14;
	        "tooltip" = false;
        };

        "custom/launcher" = {
	        "format" = "";
	        "tooltip" = false;
	        "on-click" = "bemenu-run";
        };

        "battery" =  {
	        "bat" =  "BAT1";
	        "interval" =  60;
	        "states" =  {
		        "warning" =  40;
		        "critical" =  30;
	        };
	        "format" =  "{capacity}% {icon}";
          "format-icons" = ["" "" "" "" ""];
	        "max-length" =  25;
        };

        "mpd" = {
	        "format" = "{stateIcon} {title}  ";
	        "format-disconnected" = "  ";
	        "format-stopped" = "  ";
	        "title-len" = 20;
	        "interval" = 10;
	        "on-click" = "mpc toggle";
	        "state-icons" = {
		        "paused" = "";
		        "playing" = "";
	        };
	        "tooltip-format" = "Mpd Connected";
	        "tooltip-format-disconnected" = "";
        };

	      "custom/power" = {
	        "format" = "⏻";
	        "on-click" = "d-power";
	        "tooltip" = false;
        };

        "clock" = {
	        "tooltip-format" = "{:%A %B %d %Y | %H:%M}";
	        "format-alt" = " {:%a %d %b  %I:%M %p}";
	        "format" = " {:%H:%M}";
	        ##"timezones" = [ "Kolkata" ];
	        ##"max-length" = 200;
	        "interval" = 1;
        };

        "tray" = {
          "icon-size" = 23;
          "spacing" = 10;
        };

        "cpu" = {
	        "format" = "﬙ {usage: >3}%";
	        "on-click" = "footclient -e btop";
        };

        "memory" = {
	        "format" = " {: >3}%";
	        "on-click" = "foot -e btop";
        };

        "network" = {
	        # "interface" = "wlp2s0";
	        "format" = "⚠ Disabled";
	        "format-wifi" = " {bandwidthDownBytes}  {bandwidthUpBytes}";
          "format-ethernet" = " {bandwidthDownBytes}  {bandwidthUpBytes}";
	        "format-disconnected" = "⚠ Disconnected";
	        "on-click" = "d-wifi";
	        "interval" = 2;
        };

        "pulseaudio" = {
	        "scroll-step" = 2;
	        "format" = "{icon} {volume: >3}%";
	        "format-bluetooth" = "{icon} {volume: >3}%";
	        "format-muted" =" muted";
	        "on-click" = "pamixer -t";
	        "format-icons" = {
		        "headphones" = "";
		        "handsfree" = "";
		        "headset" = "";
		        "phone" = "";
		        "portable" = "";
		        "car" = "";
		        "default" = ["" ""];
	        };
        };

        "custom/wf-recorder" = {
		      "format" = "{}";
		      "interval" = "once";
		      "exec" = "echo ''";
		      "tooltip" = "false";
		      "exec-if" = "pgrep 'wf-recorder'";
		      "on-click" = "exec d-record";
		      "signal" = 8;
	      };

      };
    };
  };

  # We will tangle config files from git repo to home dir (Let nix manage the magics)

  home.file.".config/waybar/style.css".source = config.lib.file.mkOutOfStoreSymlink "/home/idlip/d-git/d-nix/home/wayland/style.css";


}
