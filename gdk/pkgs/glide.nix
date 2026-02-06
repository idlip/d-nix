{ lib, stdenv, fetchurl,
  adwaita-icon-theme, alsa-lib, autoPatchelfHook, copyDesktopItems, curl, dbus-glib, gtk3, hicolor-icon-theme,
  libXtst, libva, mesa, makeBinaryWrapper, makeDesktopItem, patchelfUnstable, pciutils, pipewire, wrapGAppsHook3,
  ffmpeg_7, libGL, libX11, libXScrnSaver, libpciaccess, libffi, libgcrypt, libxcomposite, libxdamage, libxrandr,
  libXt, libevent, libGLU,
}:
stdenv.mkDerivation (finalAttrs: {
  pname = "glide-browser";
  version = "0.1.57a";

  src = fetchurl {
    url = "https://github.com/glide-browser/glide/releases/download/${finalAttrs.version}/glide.linux-x86_64.tar.xz";
    hash = "sha256-BK0vB8B6DN3TXs7MweiOAycB7+6IaQPau/0HvNbNta0=";
  };

  nativeBuildInputs = [
    autoPatchelfHook
    copyDesktopItems
    makeBinaryWrapper
    patchelfUnstable
    wrapGAppsHook3
  ];

  buildInputs = [
    adwaita-icon-theme
    alsa-lib
    dbus-glib
    gtk3
    hicolor-icon-theme
    libXtst
    libGL
    libX11
    libXScrnSaver
    libpciaccess
    ffmpeg_7
    libffi
    libgcrypt
    libxcomposite
    libxdamage
    libxrandr
    libXt
    alsa-lib
    libevent
    mesa
    libGLU
  ];

  runtimeDependencies = [
    curl
    libva.out
    mesa
    pciutils
    libGL
  ];

  appendRunpaths = [ "${pipewire}/lib" "${libGL}/lib" ];

  patchelfFlags = [ "--no-clobber-old-sections" ];

  preFixup = ''
    gappsWrapperArgs+=(
        --prefix LD_LIBRARY_PATH : "${lib.makeLibraryPath [ffmpeg_7]}"
      )
  '';

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin $out/share/icons/hicolor/ $out/lib/glide-browser-bin-${finalAttrs.version}
    cp -t $out/lib/glide-browser-bin-${finalAttrs.version} -r *
    chmod +x $out/lib/glide-browser-bin-${finalAttrs.version}/glide
    iconDir=$out/share/icons/hicolor
    browserIcons=$out/lib/glide-browser-bin-${finalAttrs.version}/browser/chrome/icons/default

    for i in 16 32 48 64 128; do
      iconSizeDir="$iconDir/''${i}x$i/apps"
      mkdir -p $iconSizeDir
      cp $browserIcons/default$i.png $iconSizeDir/glide-browser.png
    done

    ln -s $out/lib/glide-browser-bin-${finalAttrs.version}/glide $out/bin/glide
    ln -s $out/bin/glide $out/bin/glide-browser

    runHook postInstall
  '';

  # WebGL/Graphics settings via environment variables
  shellHook = ''
    export MOZ_DISABLE_RDD_SANDBOX=1  # Disable RDD sandbox to prevent WebGL issues
    export MOZ_WEBRENDER=1  # Enable WebRender for GPU acceleration
    export MOZ_ACCELERATED=1  # Enable hardware acceleration for WebGL
    export WebglAllowWindowsNativeGl=true  # Allow native GL for WebGL
    export AllowWebgl2=true  # Enable WebGL2 support
  '';

  desktopItems = [
    (makeDesktopItem {
      name = "glide-browser-bin";
      exec = "glide-browser --name glide-browser %U";
      icon = "glide-browser";
      desktopName = "Glide Browser";
      genericName = "Web Browser";
      terminal = false;
      startupNotify = true;
      startupWMClass = "glide-browser";
      categories = [
        "Network"
        "WebBrowser"
      ];
      mimeTypes = ["text/html" "text/xml" "application/xhtml+xml" "application/vnd.mozilla.xul+xml" "x-scheme-handler/http" "x-scheme-handler/https"];
      actions = {
        new-window = {
          name = "New Window";
          exec = "glide-browser --new-window %U";
        };
        new-private-window = {
          name = "New Private Window";
          exec = "glide-browser --private-window %U";
        };
        profile-manager-window = {
          name = "Profile Manager";
          exec = "glide-browser --ProfileManager";
        };
      };
    })
  ];

  meta = {
    changelog = "https://glide-browser.app/changelog#${finalAttrs.version}";
    description = "Extensible and keyboard-focused web browser, based on Firefox (binary package)";
    homepage = "https://glide-browser.app/";
    license = lib.licenses.mpl20;
    sourceProvenance = [ lib.sourceTypes.binaryNativeCode ];
    platforms = [ "x86_64-linux" ];
    maintainers = with lib.maintainers; [ pyrox0 idlip ];
    mainProgram = "glide-browser";
  };
})
