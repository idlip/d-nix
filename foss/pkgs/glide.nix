{ stdenv, fetchurl, steam-run, bash, undmg, ... }:

stdenv.mkDerivation rec {
  pname = "glide-browser";
  version = "0.1.54a";

  src = fetchurl {
    url = "https://github.com/glide-browser/glide/releases/download/${version}/glide.linux-x86_64.tar.xz";
    sha256 = "sha256-Rw85b+9eaiM9szWpYZiF7FqJY7OpliOwt09/c8UWlGk=";
  
  };

  sourceRoot = ".";
  nativeBuildInputs = [ undmg ];

  installPhase = ''
    mkdir -p $out/bin $out/lib/glide
  cp -r glide/* $out/lib/glide/
  chmod +x $out/lib/glide/glide

  # Create a wrapper for glide to pass arguments
  cat > $out/bin/glide <<EOF
  #!/bin/sh
  cd $out/lib/glide
  exec ${steam-run}/bin/steam-run ${bash}/bin/bash -c "GTK_IM_MODULE=\$GTK_IM_MODULE $out/lib/glide/glide \"\$@\""
  EOF
  chmod +x $out/bin/glide

  # Create a wrapper for glide-browser to pass arguments
  cat > $out/bin/glide-browser <<EOF
  #!/bin/sh
  cd $out/lib/glide
  exec ${steam-run}/bin/steam-run ${bash}/bin/bash -c "GTK_IM_MODULE=\$GTK_IM_MODULE $out/lib/glide/glide \"\$@\""
  EOF
  chmod +x $out/bin/glide-browser
  '';

  meta = with stdenv.lib; {
    description = "Glide Browser";
    homepage = "https://github.com/glide-browser/glide";
    platforms = [ "x86_64-linux" ];
  };

}
