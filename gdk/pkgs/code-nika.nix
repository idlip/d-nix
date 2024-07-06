{ lib, fetchzip, stdenvNoCC }:

stdenvNoCC.mkDerivation rec {
  pname = "code-nika-font";
  version = "0.1";

  src = fetchzip {
    url = "https://github.com/idlip/Iosevka/releases/download/0.1/code-nika.tar.gz";
    stripRoot = false;
    hash = "sha256-OqIrIPk6c7ZCymxmbVSvXJt36AMK6jyR6wcQ41V9fGQ=";
  };


  installPhase = ''
    runHook preInstall

    mkdir -p $out/share/fonts/
    mv fonts $out/share/fonts/truetype

    runHook postInstall
  '';

  meta = with lib; {
    homepage = "https://typeof.net/Iosevka/";
    description = "Custom Iosevka Font";
    license = licenses.ofl;
    maintainers = [ maintainers.idlip ];
    platforms = platforms.all;
  };
}
