{ pkgs, custom, ... }:

let
in
{
  programs.kakoune = {
    enable = true;
    extraConfig = builtins.readFile ./kakrc;
    plugins = with pkgs; [
      kak-lsp
      kakounePlugins.kak-fzf
      kakounePlugins.case-kak
      custom.pkgs.kakounePlugins.kak-smartindent
      custom.pkgs.kakounePlugins.kak-readline
      custom.pkgs.kakounePlugins.kak-mirror
      custom.pkgs.kakounePlugins.kakoune-themes
    ];
  };
}

