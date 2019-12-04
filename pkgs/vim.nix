{ config, pkgs, ... }:

{
  environment = {
    systemPackages = with pkgs; [ vim_configurable ctags ];
  };

  nixpkgs.config.vim = {
    ftNixSupport = true;
    pythonSupport = true;
  };

  programs.bash.shellAliases = {
    vi = "vim";
    svim = "sudoedit";
  };
}
