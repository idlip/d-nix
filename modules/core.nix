{
  pkgs,
  lib,
  ...
}:
# configuration shared by all hosts
{

  # enable zsh autocompletion for system packages (systemd, etc)
  environment = {
    pathsToLink = ["/share/zsh"];
    variables = {
      EDITOR = "emacsclient -nw -a 'nvim'";
      BROWSER = "brave";
      NIXOS_OZONE_WL = "1";
    };
    systemPackages = with pkgs; [
      git
      (writeScriptBin "sudo" ''exec doas "$@"'')
    ];
  };


  i18n = {
    defaultLocale = "en_US.UTF-8";
    # saves space
    supportedLocales = [
      "en_US.UTF-8/UTF-8"
      "ja_JP.UTF-8/UTF-8"
      "ro_RO.UTF-8/UTF-8"
    ];
  };

  # Sets big font for bootloader, as I have small laptop.
  # You can remove font and packages line to have default font kernel chooses.
  console = {
    earlySetup = true;
    font = "${pkgs.terminus_font}/share/consolefonts/ter-132n.psf.gz";
    packages = with pkgs; [ terminus_font ];
    keyMap = "us";
  };

  # graphics drivers / HW accel
  hardware.opengl.enable = true;

  # enable programs
  programs = {
    less.enable = true;
    # type "fuck" to fix the last command that made you go "fuck"
    thefuck.enable = true;

    # allow users to mount fuse filesystems with allow_other
    fuse.userAllowOther = true;

    # help manage android devices via command line
    adb.enable = true;

    bash.promptInit = ''eval "$(${pkgs.starship}/bin/starship init bash)"'';

    zsh = {
      enable = true;
      autosuggestions.enable = true;
      syntaxHighlighting = {
        enable = true;
        patterns = {"rm -rf *" = "fg=black,bg=red";};
        styles = {"alias" = "fg=magenta";};
        highlighters = ["main" "brackets" "pattern"];
      };
    };
  };

  # obviously your timezone here. Have a nice day or good night sleep ;)
  # Don't waste more time on nixos lol, be healthy and have some sleep. Stay healthy!
  time = {
    timeZone = "Asia/Kolkata";
    hardwareClockInLocalTime = true;
  };

  users.users.idlip = {
    isNormalUser = true;
    shell = pkgs.zsh;
    extraGroups = ["adbusers" "input" "libvirtd" "networkmanager" "plugdev" "transmission" "video" "wheel"];
  };

  # compresses half the ram for use as swap
  zramSwap = {
    enable = true;
    memoryPercent = 50;
    algorithm = "zstd";
  };

  # DON'T TOUCH THIS
  system.stateVersion = lib.mkDefault "23.05";

}
