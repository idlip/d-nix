{
  pkgs,
  ...
}: {

  services.dunst = {
    enable = true;
    iconTheme = {
      package = pkgs.papirus-icon-theme;
      name = "Papirus";
    };
    settings = {
      global = {
        monitor = 0;
        background = "#050505";
        frame_color = "#2e8b57";
        transparency = 0;
        follow = "none";
        width = 900;
        height = 900;
        idle_threshold = 120;
        origin = "top-right";
        offset = "10x50";
        scale = 0;
        notification_limit = 0;
        progress_bar = "true";
        alignment = "center";
        progress_bar_height = 10;
        progress_bar_frame_width = 1;
        progress_bar_min_width = 150;
        progress_bar_max_width = 500;
        indicate_hidden = "yes";
        separator_height = 2;
        padding = 20;
        horizontal_padding = 12;
        text_icon_padding = 8;
        frame_width = 3;
        separator_color = "frame";
        sort = "yes";
        font = "Code D Ace 20";
        line_height = 0;
        markup = "full";
        stack_duplicates = "true";
        vertical_alignment = "center";
        show_age_threshold = 60;
        ellipsize = "middle";
        ignore_newline = "no";
        show_indicators = "yes";
        icon_position = "left";
        max_icon_size = 32;
        sticky_history = "yes";
        history_length = 20;
        browser = "d-stuff";
        always_run_script = "true";
        title = "Dunst";
        class = "Dunst";
        corner_radius = 20;
        ignore_dbusclose = false;
        force_xwayland = "false";
        layer = "overlay";
        mouse_left_click = "do_action";
        mouse_middle_click = "do_action";
        mouse_right_click = "close_all";
      };

      reminder = {
        category = "reminder";
        background = "#33333390";
        foreground = "#ffffff";
        timeout = 0;
        script="d-notif";
      };

      urgency_low = {
        background = "#050505";
        foreground = "#ffffff";
        timeout = 5;
      };
      urgency_normal = {
        background = "#050505";
        foreground = "#ffffff";
        timeout = 6;
      };
      urgency_critical = {
        background = "#050505";
        foreground = "#ffffff";
        frame_color = "#f38ba8";
        timeout = 0;
      };
    };
  };

}
