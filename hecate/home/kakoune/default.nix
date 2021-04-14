{ pkgs, inputs, ... }:

let
in {
  programs.kakoune = {
    enable = true;
    extraConfig = builtins.readFile ./kakrc;
    plugins = with pkgs; [
      kak-lsp
      kakounePlugins.kak-fzf
      kakounePlugins.case-kak
      inputs.kakounePlugins.kak-smartindent
      inputs.kakounePlugins.kak-readline
      inputs.kakounePlugins.kak-mirror
    ];
  };
}

