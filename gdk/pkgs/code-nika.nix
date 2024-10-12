{ lib, fetchzip, stdenvNoCC }:

stdenvNoCC.mkDerivation rec {
  pname = "code-nika-font";
  version = "0.4";

  src = fetchzip {
    url = "https://github.com/idlip/Iosevka/releases/download/${version}/code-nika.tar.gz";
    stripRoot = false;
    hash = "sha256-nG2SO4iR6vzipgjAcrHlsbM7+39UIDI4RrojdhHoY28=";
  };


  installPhase = ''
    runHook preInstall

    fontdir="$out/share/fonts/truetype"
    install -d "$fontdir"
    install Code{Haki,OnePiece}/* "$fontdir"

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
