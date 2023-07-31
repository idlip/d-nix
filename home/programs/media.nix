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
      network = {
        listenAddress = "any";
        port = 6600;
      };
      musicDirectory = "~/d-sync/music";
      extraConfig = ''
        audio_output {
      	  type    "pipewire"
        	name    "pipewire"
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

  ];


  programs= {
    ncmpcpp = {
      enable = true;
      settings = {
        ncmpcpp_directory = "~/.config/ncmpcpp";
        lyrics_directory = "~/.local/share/lyrics";
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
        msg-level = "all = warn";
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
        options.background = "ffffff";
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

}
