{ pkgs, npkgs, ... }:

let
in {
  programs.kakoune = {
    enable = true;
    extraConfig = builtins.readFile ./kakrc;
    plugins = with pkgs; [
      kak-lsp
      kakounePlugins.kak-fzf
      kakounePlugins.case-kak
      npkgs.kakounePlugins.kak-smartindent
      npkgs.kakounePlugins.kak-readline
      npkgs.kakounePlugins.kak-mirror
      npkgs.kakounePlugins.kakoune-themes
    ];
  };
}

