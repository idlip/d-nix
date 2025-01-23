{ lib, fetchzip, stdenvNoCC }:

stdenvNoCC.mkDerivation rec {
  pname = "code-nika-font";
  version = "0.5";

  src = fetchzip {
    url = "https://github.com/idlip/Iosevka/releases/download/${version}/code-nika.zip";
    stripRoot = false;
    hash = "sha256-IeVKwmTLl2+UDvMDpCx8FELuCHJ7IxjIlDidKE7It/M=";
  };


  installPhase = ''
    runHook preInstall

    install -dm 0755 $out/share/fonts/truetype
    cp -rf Code*/* $out/share/fonts/truetype

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
