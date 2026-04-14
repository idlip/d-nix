{
  lib,
  stdenvNoCC,
  fetchFromGitHub,
}:

stdenvNoCC.mkDerivation rec {
  pname = "layan-cursors";
  version = "2021-08-01";

  src = fetchFromGitHub {
    owner = "vinceliuice";
    repo = "Layan-cursors";
    rev = "${version}";
    hash = "sha256-Izc5Q3IuM0ryTIdL+GjhRT7JKbznyxS2Fc4pY5dksq4=";
  };

  installPhase = ''
    runHook preInstall

    install -dm 0755 $out/share/icons
    cp -rf dist $out/share/icons/layan-cursors

    runHook postInstall
  '';

}
