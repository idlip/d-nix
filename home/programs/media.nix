{
  pkgs,
  config,
  ...
}:
# media - control and enjoy audio/video
{

  services = {
    mpd = {
      enable = true;
      musicDirectory = config.xdg.userDirs.music;
      network = {
        listenAddress = "127.0.0.1";
        port = 6600;
        startWhenNeeded = true;
      };
      extraConfig = ''
      audio_output {
        type            "pipewire"
        name            "PipeWire Sound Server"
      }
      auto_update "yes"
    '';
    };

    playerctld.enable = true;

  };

  home.packages = with pkgs; [
    # audio control
    pavucontrol
    playerctl
    pulsemixer
    pamixer

    mpc_cli
    ytfzf ani-cli youtube-tui
    mangal
  ];


  programs= {
    ncmpcpp = {
      enable = true;

      settings = {
        ncmpcpp_directory = "${config.xdg.configHome}/ncmpcpp";
        lyrics_directory = "${config.xdg.dataHome}/lyrics";
        message_delay_time = "1";
        song_list_format = "{$4%a - }{%t}|{$8%f$9}$R{$3(%l)$9}";
        song_status_format = "$b{{$8'%t'}} $3by {$4%a{ $3in $7%b{ (%y)}} $3}|{$8%f}";
        song_library_format = "{%n - }{%t}|{%f}";
        alternative_header_first_line_format = "$b$1$aqqu$/a$9 {%t}|{%f} $1$atqq$/a$9$/b";
        alternative_header_second_line_format = "{{$4$b%a$/b$9}{ - $7%b$9}{ ($4%y$9)}}|{%D}";
        current_item_prefix = "$(cyan)$r$b";
        current_item_suffix = "$/r$(end)$/b";
        current_item_inactive_column_prefix = "$(magenta)$r";
        current_item_inactive_column_suffix = "$/r$(end)";
        playlist_display_mode = "columns";
        browser_display_mode = "columns";
        progressbar_look = "->";
        media_library_primary_tag = "album_artist";
        media_library_albums_split_by_date = "no";
        startup_screen = "media_library";
        display_volume_level = "no";
        ignore_leading_the = "yes";
        external_editor = "nvim";
        use_console_editor = "yes";
        empty_tag_color = "magenta";
        main_window_color = "white";
        progressbar_color = "black:b";
        progressbar_elapsed_color = "blue:b";
        statusbar_color = "red";
        statusbar_time_color = "cyan:b";
      };

      bindings =
        [
          { key = "K"; command = [ "select_item" "scroll_up" ]; }
          { key = "+"; command = "show_clock"; }
          { key = "="; command = "volume_up"; }
          { key = "j"; command = "scroll_down"; }
          { key = "k"; command = "scroll_up"; }
          { key = "ctrl-u"; command = "page_up"; }
          { key = "ctrl-d"; command = "page_down"; }
          { key = "u"; command = "page_up"; }
          { key = "d"; command = "page_down"; }
          { key = "h"; command = [ "previous_column" "jump_to_parent_directory" ]; }
          { key = "l"; command = [ "next_column" "enter_directory" "run_action" "play_item" ]; }
          { key = "."; command = "show_lyrics"; }
          { key = "n"; command = "next_found_item"; }
          { key = "N"; command = "previous_found_item"; }
          { key = "J"; command = "move_sort_order_down"; }
          { key = "K"; command = "move_sort_order_up"; }
          { key = "m"; command = [ "show_media_library" "toggle_media_library_columns_mode" ]; }
          { key = "t"; command = "show_tag_editor"; }
          { key = "v"; command = "show_visualizer"; }
          { key = "G"; command = "move_end"; }
          { key = "g"; command = "move_home"; }
          { key = "U"; command = "update_database"; }
          { key = "s"; command = [ "reset_search_engine" "show_search_engine" ]; }
          { key = "f"; command = [ "show_browser" "change_browse_mode" ]; }
          { key = "x"; command = "delete_playlist_items"; }
          { key = "P"; command = "show_playlist"; }
        ];

    };

    mpv = {
      enable = true;
      # scripts = with pkgs.mpvScripts; [ thumbnail sponsorblock];

      bindings = {
        l = "seek  5";
        h = "seek -5";
        k = "seek  30";
        j = "seek -30";
        J = "cycle sub";
        K = "cycle sub down";
      };

      config = {
        force-window = true;
        osc = false;
        sub-border-size = 4.0;
        sub-shadow-color = "0.0/0.0/0.0";
        sub-shadow-offset = 1;
        sub-auto = "fuzzy";
        msg-level = "all=error";
        ytdl-format = "[height<1080]";
        save-position-on-quit = true;
        slang = "eng,en,Eng,English";
        alang = "jp,jpn,en,eng";
        sub-font = "Impress BT";
        autofit = "50%";
        sub-font-size = "48";
        speed = "1.5";
      };
    };

    imv = {
      enable = true;
      settings = { # check man imv(5))
        options.background = "050505";
        aliases.x = "close";
        binds = {
          p = "prev";
          n = "next";
          "Shift+d" = "exec rm \"$imv_current_file\"; close";
          r = "exec mogrify -rotate 90 \"$imv_current_file\"";
        };
      };
    };

    yt-dlp = {
      enable = true;
      settings = {
        embed-thumbnail = true;
        embed-metadata = true;
        embed-subs = true;
        sub-langs = "all";
      };
    };
  };

  xdg.configFile."ytfzf/conf.sh".text = ''
      #video_pref="248+bestaudio/best"
      video_pref="[height<=1080]"
      sub_link_count=1
      show_thumbnails=0

      external_menu () {
           bemenu -W 0.98 -l 24 -p '  Play '
      }

      thumbnail_quality=high
      scrape=youtube
      #is_sort=1
      #search_sort_by=upload_date

    '';
 home.sessionVariables.MPD_HOST = config.services.mpd.network.listenAddress;

}
