{config, ...}: {
  home.sessionVariables.STARSHIP_CACHE = "${config.xdg.cacheHome}/starship";

  programs.starship = {
    enable = true;
    enableZshIntegration = true;

    settings = {
      add_newline = true;
      scan_timeout = 5;

      line_break.disabled = false;

      hostname = {
	      ssh_only = true;
	      format = "[$hostname](bold blue) ";
	      disabled = false;
      };
    };
  };
}
