{
  lib,
  stdenvNoCC,
  # fetchFromGitHub,
  fetchurl,
  nix-update-script,
}:

stdenvNoCC.mkDerivation (finalAttrs: {
  pname = "NieR-Cursors";
  version = "2020-08-25";

  # src = fetchFromGitHub {
  #   owner = "Beinsezii";
  #   repo = "NieR-Cursors";
  #   tag = finalAttrs.version;
  #   hash = "sha256-6zINpfRVjRM27wPpGTQplDPJCwHGn6GijfNqR+FKyDo=";
  # };

  src = fetchurl {
    url = "https://github.com/Beinsezii/NieR-Cursors/releases/download/2020-08-25/NieR_Cursors_2020-08-25.tar.xz";
    hash = "sha256-Smkf/A+HDuJ85f3dx1WIQ+4pHyVpJvi/A8aZoFKS8yQ=";
  };

  sourceRoot = ".";
  installPhase = ''
    runHook preInstall
    mkdir -p $out/share/icons
    cp -R nier_cursors $out/share/icons/
    runHook postInstall
  '';

  passthru.updateScript = nix-update-script { };

  meta = {
    description = "Cursor theme inspired by layan gtk theme and based on capitaine-cursors";
    changelog = "https://github.com/vinceliuice/Layan-cursors/releases/tag/${finalAttrs.version}/CHANGELOG.md";
    homepage = "https://github.com/vinceliuice/Layan-cursors/";
    license = lib.licenses.gpl3Only;
    maintainers = with lib.maintainers; [ idlip ];
  };

})
