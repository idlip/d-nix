{ lib, fetchgit, stdenvNoCC }:

stdenvNoCC.mkDerivation rec {
  pname = "code-d-font";
  version = "1.085";

  src = fetchgit {
    url = "https://github.com/idlip/code-d-font";
    sparseCheckout = [
      "fonts"
    ];
    sha256 = "sha256-PkJOX+zYQYs0J+T92y4oTXsYXKJ960/TSELDjykw52Q=";
  };

  installPhase = ''
    install -D -t $out/share/fonts/truetype/ $(find $src -type f -name '*.ttf')
  '';


  meta = with lib; {
    homepage = "https://github.com/arrowtype/recursive-code-config/";
    description = "A variable font family for code & UI";
    license = licenses.ofl;
    maintainers = [ maintainers.idlip ];
    platforms = platforms.all;
  };
}
