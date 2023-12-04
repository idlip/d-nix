{
  ...
}: {

  # configuring syncthing
  services.syncthing = {
    enable = true;
    user = "idlip";
    configDir = "/home/idlip/.config/syncthing";
    dataDir = "/home/idlip/.local/share/syncthing";
    overrideDevices = true;     # overrides any devices added or deleted through the WebUI
    overrideFolders = true;     # overrides any folders added or deleted through the WebUI
    settings = {
      devices = {
        "realme" = { id = "CEV3U3M-EJFLUJ3-UXFBEPG-KHX5EVK-3MSYH2W-BRNZEDH-TVJ4QWZ-X3G2CAW"; };
        #"device2" = { id = "DEVICE-ID-GOES-HERE"; };
      };
      folders = {
        "sync" = {
	        path = "~/d-sync";
	        devices = [ "realme" ];
        };
        "emacs" = {
	        path = "~/d-git/d-nix";
	        devices = [ "realme" ];
        };
        "theme" = {
	        path = "~/d-git/d-theme";
	        devices = [ "realme" ];
        };
        "site" = {
	        path = "~/d-git/d-site";
	        devices = [ "realme" ];
        };
      };
    };
  };

}
