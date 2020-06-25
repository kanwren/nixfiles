{ pkgs, ... }:

let
  myVim = with pkgs; vim_configurable.override {
    python = python3;
  };
in
{
  environment = {
    systemPackages = [
      myVim
      pkgs.rnix-lsp # nix lsp via rnix
    ];
    shellAliases.vi = "vim";
    variables = {
      # Make vim the default editor
      EDITOR = "vim";
      VISUAL = "vim";
    };
  };

  programs = {
    bash.shellAliases = {
      vi = "vim";
      svim = "sudoedit";
    };
  };
}
