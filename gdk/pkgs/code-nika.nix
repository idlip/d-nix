{ lib, fetchzip, stdenvNoCC }:

stdenvNoCC.mkDerivation rec {
  pname = "code-nika-font";
  version = "0.3";

  src = fetchzip {
    url = "https://github.com/idlip/Iosevka/releases/download/${version}/code-nika.tar.gz";
    stripRoot = false;
    hash = "sha256-ZXWZVGOCTnVnGSkOWrQx8HEuJYdnGb3IbFHO3UXxnps=";
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
