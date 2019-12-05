{ config, pkgs, ... }:

{
  environment = {
    systemPackages = with pkgs; [ vim_configurable ctags ];
    shellAliases.vi = "vim";
    variables.EDITOR = "vim";
    variables.VISUAL = "vim";
  };

  nixpkgs.config.vim = {
    ftNixSupport = true;
    pythonSupport = true;
  };

  programs.bash.shellAliases = {
    vi = "vim";
    svim = "sudo -E vim"; # or sudoedit
  };
}
