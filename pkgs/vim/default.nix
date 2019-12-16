{ config, pkgs, ... }:

let
  myVim = with pkgs; vim_configurable.override {
    python = python3;
  };
in
{
  environment = {
    systemPackages = with pkgs; [ myVim ctags ];
    shellAliases.vi = "vim";
    variables = {
      EDITOR = "vim";
      VISUAL = "vim";
    };
  };

  nixpkgs.config.vim = {
    python = true;
  };

  programs = {
    vim.defaultEditor = true;
    bash.shellAliases = {
      vi = "vim";
      svim = "sudo -E vim"; # or sudoedit
    };
  };
}
