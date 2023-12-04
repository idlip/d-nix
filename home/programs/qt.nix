{pkgs, ...}:
# Qt theming with Kvantum
{

  qt = {
    enable = true;
    platformTheme = "gtk3";
    style.name = "adwaita-dark";
  };

  services.kdeconnect.enable = true;

}
