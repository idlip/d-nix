{pkgs, ...}: {
  home.packages = with pkgs; [
    # misc
    libnotify
    xdg-utils

    pcmanfm
    libreoffice pandoc groff mupdf

    # pioneer of web
    mullvad-browser librewolf ungoogled-chromium
  ];
}
