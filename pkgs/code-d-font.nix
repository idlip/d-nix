{ lib, fetchFromGitHub, stdenvNoCC }:

stdenvNoCC.mkDerivation rec {
  pname = "code-d-font";
  version = "0.1";

  src = fetchFromGitHub {
    owner = "idlip";
    repo = "code-d-font";
    rev = "ea588e0890037719962ed49ccbfdc5381d9ab7f4";
    sparseCheckout = [
      "fonts"
    ];
    sha256 = "sha256-+0yK0CEZsXJGoOxJY12ARrHvmWhaQGER+BXi0xzR7uk=";
  };

  installPhase = ''
    runHook preInstall

    mkdir -p $out/share/fonts
    mv fonts $out/share/fonts/truetype

    runHook postInstall
  '';

  meta = with lib; {
    homepage = "https://github.com/arrowtype/recursive-code-config/";
    description = "A variable font family for code & UI";
    license = licenses.ofl;
    maintainers = [ maintainers.idlip ];
    platforms = platforms.all;
  };
}
