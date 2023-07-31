{
  config,
  pkgs,
  lib,
  ...
}: {

  home.file.".config/shell/sources.sh".source = config.lib.file.mkOutOfStoreSymlink "/home/idlip/d-git/d-nix/home/shell/sources.sh";

  programs.zsh = {
    enable = true;
    dotDir = ".config/shell";
    autocd = true;
    enableCompletion = true;
    enableAutosuggestions = true;
    history.extended = true;

    dirHashes = {
      dl = "$HOME/dloads";
      docs = "$HOME/docs";
      code = "$HOME/d-git/codes";
      dots = "$HOME/d-git/d-nix";
      pics = "$HOME/pics";
      vids = "$HOME/vids";
      nixpkgs = "$HOME/d-git/forks/nixpkgs";
    };

    envExtra = ''
    export PATH="$PATH:/home/idlip/d-git/d-bin/bin:$HOME/.local/bin"
  	export EDITOR="emacsclient -nw -a 'nvim'"
    export BEMENU_OPTS="-i -s -l 10 -R 20 -p ' ' -c -B 2 -W 0.5 --hp 15 --fn 'Code D Ace 20' --nb '#121212' --ab '#121212' --bdr '#c6daff' --nf '#ffffff' --af '#ffffff' --hb '#9aff9a' --hf '#121212' --fb '#121212' --ff '#a6e3a1' --tb '#121212' --tf '#f9e2af' ";
    export VISUAL=$EDITOR
    export STARDICT_DATA_DIR="$HOME/.local/share/stardict"
    export GRIM_DEFAULT_DIR="$HOME/pics/sshots/"

    	if [ -z $DISPLAY ] && [ "$(tty)" = "/dev/tty1" ]; then
        exec Hyprland
     	fi
  	'';

    initExtra = ''
	source ~/.config/shell/sources.sh

	source "${pkgs.zsh-syntax-highlighting}/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh"
	source "${pkgs.zsh-history-substring-search}/share/zsh-history-substring-search/zsh-history-substring-search.zsh"
	source "${pkgs.zsh-autosuggestions}/share/zsh-autosuggestions/zsh-autosuggestions.zsh"
	source "${pkgs.nix-zsh-completions}/share/zsh/plugins/nix/nix-zsh-completions.plugin.zsh"
	source "${pkgs.zsh-nix-shell}/share/zsh-nix-shell/nix-shell.plugin.zsh"
	source "${pkgs.zsh-autopair}/share/zsh/zsh-autopair/autopair.zsh"

	'';

    history = {
      save = 10000;
      size = 10000;
      expireDuplicatesFirst = true;
      ignoreDups = true;
      path = "${config.xdg.dataHome}/zsh_history";
    };
  };

}
