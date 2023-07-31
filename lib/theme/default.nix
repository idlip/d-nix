{
  colorlib,
  lib,
}: rec {

  colors = import ./colors.nix;

  # #RRGGBB
  xcolors = lib.mapAttrs (_: colorlib.x) colors;

  # rgba(,,,) colors (css)
  rgbaColors = lib.mapAttrs (_: colorlib.rgba) colors;

  browser = "brave";

  launcher = "bemenu";

  terminal = {
    font = "Code D OnePiece";
    opacity = 0.9;
    size = 12;
  };

  wallpaper = builtins.fetchurl rec {
    name = "wallpaper-${sha256}.png";
    url = "https://w.wallhaven.cc/full/1p/wallhaven-1pd1o9.jpg";
    sha256 = "1xngx610skv1vqzx1c7j2zv5cg3gld3hkcxki8jd30rssjjx98p2";
  };
}
