{ pkgs, ... }:

{
  home-manager.users.nprin.programs.vscode = {
    enable = true;

    package = pkgs.vscodium;

    # userSettings = {
    #   "editor.fontSize" = 18;
    #   "[nix]"."editor.tabSize" = 2;
    #   "[hs]"."editor.tabSize" = 2;
    #   "vim.useSystemClipboard" = true;
    #   "vim.incsearch" = true;
    #   "vim.hlsearch" = true;
    #   "vim.useCtrlKeys" = true;
    #   "vim.visualstar" = true;
    # };

    extensions = with pkgs.vscode-extensions; [
      vscodevim.vim
    ];
  };
}
