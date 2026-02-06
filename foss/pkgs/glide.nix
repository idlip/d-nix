{
  lib,
  stdenv,
  fetchurl,
  # Build/Packaging Tools
  autoPatchelfHook,
  copyDesktopItems,
  makeBinaryWrapper,
  makeDesktopItem,
  patchelfUnstable,
  wrapGAppsHook3,
  nix-update-script,
  # Core Libs
  alsa-lib,
  at-spi2-atk,
  atk,
  cairo,
  curl,
  dbus,
  dbus-glib,
  ffmpeg_7,
  gdk-pixbuf,
  glib,
  gsettings-desktop-schemas,
  gtk3,
  libcanberra-gtk3,
  libGL,
  libdrm,
  libgbm,
  libnotify,
  libpulseaudio,
  librsvg,
  libva,
  libxkbcommon,
  mesa,
  pango,
  pciutils,
  pipewire,
  speechd-minimal,
  udev,
  vulkan-loader,
  wayland,
  xorg,
  ...
}:
let
  appId = "glide-browser";

  # These libraries are dlopen()'ed by the browser executable at runtime.
  # They MUST be in LD_LIBRARY_PATH for features to work.
  runtimeLibs = [
    # Core GUI & IPC
    libGL
    libcanberra-gtk3
    libdrm
    libgbm
    libnotify
    libxkbcommon
    wayland
    dbus
    dbus-glib
    gtk3
    glib
    cairo
    pango
    gdk-pixbuf
    atk
    at-spi2-atk

    # Media (Microphone & Audio)
    pipewire
    libpulseaudio
    alsa-lib
    speechd-minimal

    # Hardware Acceleration & Codecs
    ffmpeg_7
    libva
    mesa
    vulkan-loader
    udev

    # X11 Compatibility
    xorg.libX11
    xorg.libXcomposite
    xorg.libXdamage
    xorg.libXext
    xorg.libXfixes
    xorg.libXrandr
    xorg.libxcb
  ];
in
stdenv.mkDerivation (finalAttrs: {
  pname = "glide-browser";
  version = "0.1.58a";

  src = fetchurl {
    url = "https://github.com/glide-browser/glide/releases/download/${finalAttrs.version}/glide.linux-x86_64.tar.xz";
    sha256 = "sha256-yut/yXT+BJCFackLSRG7tLBD6m008k0lC62Qwt7aRX8=";
  };

  nativeBuildInputs = [
    copyDesktopItems
    makeBinaryWrapper
  ]
  ++ lib.optionals stdenv.isLinux [
    autoPatchelfHook
    patchelfUnstable
    wrapGAppsHook3
  ];

  # Provide strict superset of libs for autoPatchelf to resolve symbols
  buildInputs = lib.optionals stdenv.isLinux (
    runtimeLibs
    ++ [
      speechd-minimal
      pciutils
      curl
      gsettings-desktop-schemas
      librsvg
    ]
  );

  # Ensure patchelf doesn't miss these
  runtimeDependencies = lib.optionals stdenv.isLinux runtimeLibs;

  appendRunpaths = lib.optionals stdenv.isLinux [
    "${lib.getLib pipewire}/lib"
    "${lib.getLib libGL}/lib"
    "${lib.getLib udev}/lib"
  ];

  patchelfFlags = lib.optionals stdenv.isLinux [ "--no-clobber-old-sections" ];

  preFixup = lib.optionalString stdenv.isLinux ''
    gappsWrapperArgs+=(
      # Explicitly inject runtime libraries. 
      --prefix LD_LIBRARY_PATH : "${lib.makeLibraryPath runtimeLibs}"

      --set MOZ_APP_LAUNCHER "${appId}"
      --set MOZ_LEGACY_PROFILES 1
      --set MOZ_ALLOW_DOWNGRADE 1
      --set-default MOZ_ENABLE_WAYLAND 1
      
      # Required for the window manager to associate the window correctly
      --add-flags "--name=${appId}"
      --add-flags "--class=${appId}"
    )
  '';

  unpackPhase = lib.optionalString stdenv.isDarwin ''
    runHook preUnpack
    /usr/bin/hdiutil attach -nobrowse -readonly $src
    cp -r /Volumes/Glide/Glide.app .
    /usr/bin/hdiutil detach /Volumes/Glide
    runHook postUnpack
  '';

  installPhase =
    if stdenv.isLinux then
      ''
        runHook preInstall

        mkdir -p $out/bin $out/share/icons/hicolor/ $out/lib/glide-browser-bin-${finalAttrs.version}
        cp -t $out/lib/glide-browser-bin-${finalAttrs.version} -r *

        # Ensure binaries are executable for patchelf
        chmod +x $out/lib/glide-browser-bin-${finalAttrs.version}/glide

        iconDir=$out/share/icons/hicolor
        browserIcons=$out/lib/glide-browser-bin-${finalAttrs.version}/browser/chrome/icons/default

        for i in 16 32 48 64 128; do
          iconSizeDir="$iconDir/''${i}x$i/apps"
          mkdir -p $iconSizeDir
          cp $browserIcons/default$i.png $iconSizeDir/${appId}.png
        done

        ln -s $out/lib/glide-browser-bin-${finalAttrs.version}/glide $out/bin/glide
        ln -s $out/bin/glide $out/bin/${appId}

        runHook postInstall
      ''
    else
      ''
        runHook preInstall
        mkdir -p $out/Applications
        cp -r Glide.app $out/Applications/
        mkdir -p $out/bin
        ln -s $out/Applications/Glide.app/Contents/MacOS/glide $out/bin/glide
        ln -s $out/bin/glide $out/bin/${appId}
        runHook postInstall
      '';

  desktopItems = [
    (makeDesktopItem {
      name = appId;
      exec = "${appId} --name ${appId} %U";
      icon = appId;
      desktopName = "Glide Browser";
      genericName = "Web Browser";
      terminal = false;
      startupNotify = true;
      startupWMClass = appId;
      categories = [
        "Network"
        "WebBrowser"
      ];
      mimeTypes = [
        "text/html"
        "text/xml"
        "application/xhtml+xml"
        "application/vnd.mozilla.xul+xml"
        "x-scheme-handler/http"
        "x-scheme-handler/https"
      ];
      actions = {
        new-window = {
          name = "New Window";
          exec = "${appId} --new-window %U";
        };
        new-private-window = {
          name = "New Private Window";
          exec = "${appId} --private-window %U";
        };
        profile-manager-window = {
          name = "Profile Manager";
          exec = "${appId} --ProfileManager";
        };
      };
    })
  ];

  passthru.updateScript = nix-update-script {
    extraArgs = [
      "--url"
      "https://github.com/glide-browser/glide"
    ];
  };

  meta = {
    changelog = "https://glide-browser.app/changelog#${finalAttrs.version}";
    description = "Extensible and keyboard-focused web browser, based on Firefox (binary package)";
    homepage = "https://glide-browser.app/";
    license = lib.licenses.mpl20;
    sourceProvenance = [ lib.sourceTypes.binaryNativeCode ];
    platforms = [
      "x86_64-linux"
      "aarch64-linux"
      "x86_64-darwin"
      "aarch64-darwin"
    ];
    maintainers = with lib.maintainers; [ pyrox0 ];
    mainProgram = appId;
  };
})
