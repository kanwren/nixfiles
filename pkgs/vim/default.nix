{ pkgs, ... }:

let
  myVim = with pkgs; vim_configurable.override {
    python = python3;
  };
in
{
  environment = {
    systemPackages = [ myVim ];
    shellAliases.vi = "vim";
    variables = {
      # Make vim the default editor
      EDITOR = "vim";
      VISUAL = "vim";
    };
  };

  programs = {
    vim.defaultEditor = true;
    bash.shellAliases = {
      vi = "vim";
      svim = "sudoedit";
    };
  };
}
