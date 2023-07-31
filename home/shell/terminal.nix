{pkgs, ...}: {

  programs.foot = {
    enable = true;

    # doesnt work properly; Enable it in hyprland or sway config
    server.enable = false;

    settings = {
      main = {
        term = "xterm-256color";
        font = "Code D OnePiece:size=12, Noto Color Emoji:size=15";
        font-bold = "Code D Lip:size=12, Noto Color Emoji:size=15";
        letter-spacing = "1";
        box-drawings-uses-font-glyphs = "yes";
        pad = "0x0center";
        notify = "notify-send -a \${app-id} -i \${app-id} \${title} \${body}";
        selection-target = "clipboard";
	dpi-aware = "true";
      };

      scrollback = {
        lines = 10000;
        multiplier = 3;
      };

      url = {
        launch = "d-stuff \${url}";
        label-letters = "sadfjklewcmpgh";
        osc8-underline = "url-mode";
        protocols = "http, https, ftp, ftps, file";
        uri-characters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_.,~:;/?#@!$&%*+=\"'()[]";
      };

      key-bindings = {
        clipboard-copy = "Control+Shift+c Mod1+w";
        clipboard-paste = "Control+Shift+v Control+y";
        primary-paste = "Shift+Insert";

      };

      colors = {
        background = "050505";
        foreground = "ffffff";
        regular0 = "030303";
        regular1 = "ff8059";
        regular2 = "44bc44";
        regular3 = "d0bc00";
        regular4 = "2fafff";
        regular5 = "feacd0";
        regular6 = "00d3d0";
        regular7 = "bfbfbf";
        bright0 = "595959";
        bright1 = "ef8b50";
        bright2 = "70b900";
        bright3 = "c0c530";
        bright4 = "79a8ff";
        bright5 = "b6a0ff";
        bright6 = "6ae4b9";
        bright7 = "ffffff";
        alpha = "1.0";
      };

      mouse = {
        hide-when-typing = "yes";
      };

    };
  };
}
