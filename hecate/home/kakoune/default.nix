{ pkgs, ... }:

{
  programs.kakoune = {
    enable = true;
    extraConfig = builtins.readFile ./kakrc;
    plugins = with pkgs.kakounePlugins; [
      pkgs.kak-lsp
      kak-fzf
    ];
  };
}

