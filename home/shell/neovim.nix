{pkgs, ...}: {

  programs.neovim = {
    enable = true;

    vimAlias = true;
    viAlias = true;
    vimdiffAlias = true;
    withPython3 = true;

    extraPackages = with pkgs; [gcc ripgrep fd];

  };

}
