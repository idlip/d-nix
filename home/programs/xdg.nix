{config, ...}: let
  browser = ["d-stuff.desktop"];

  # XDG MIME types
  associations = {
    "application/x-extension-htm" = browser;
    "application/x-extension-html" = browser;
    "application/x-extension-shtml" = browser;
    "application/x-extension-xht" = browser;
    "application/x-extension-xhtml" = browser;
    "application/xhtml+xml" = browser;
    "text/html" = browser;
    "x-scheme-handler/about" = browser;
    "x-scheme-handler/chrome" = ["chromium-browser.desktop"];
    "x-scheme-handler/ftp" = browser;
    "x-scheme-handler/http" = browser;
    "x-scheme-handler/https" = browser;
    "x-scheme-handler/unknown" = browser;

    "audio/*" = ["mpv.desktop"];
    "video/*" = ["mpv.dekstop"];
    "image/*" = ["imv.desktop"];
    "application/json" = browser;
    "application/pdf" = ["sioyek.desktop"];
    "x-scheme-handler/magnet" = ["d-stuff.desktop"];
    "application/epub+zip" = ["sioyek.desktop"];
    "application/zip" = ["sioyek.desktop"];
    "application/x.bittorrent" = ["d-stuff.desktop"];
  };

in {

  xdg = {
    enable = true;
    cacheHome = config.home.homeDirectory + "/.local/cache";

    mimeApps = {
      enable = true;
      defaultApplications = associations;
      associations.added = {
        "x-scheme-handler/magnet" = ["d-stuff.desktop"];
      };
    };

    userDirs = {
      enable = true;
      createDirectories = true;
      documents =  "${config.home.homeDirectory}/docs";
      download = "${config.home.homeDirectory}/dloads";
      videos = "${config.home.homeDirectory}/vids";
      music = "${config.home.homeDirectory}/music";
      pictures = "${config.home.homeDirectory}/pics";
      extraConfig = {
        XDG_SCREENSHOTS_DIR = "${config.xdg.userDirs.pictures}/sshots";
      };
    };

  };

  xdg.dataFile."applications/d-stuff.desktop".text = ''
  [Desktop Entry]
  Type=Application
  Name=Stuff Handler
  Exec=d-stuff %U
  '';

}
