{ pkgs, ... }:

let
  kak-smartindent = pkgs.stdenv.mkDerivation {
    name = "kak-smartindent";
    src = pkgs.fetchFromGitHub {
      owner = "andreyorst";
      repo = "smarttab.kak";
      rev = "6e3264df7a9a786961e634d5ae8fb117ca86ef0a";
      sha256 = "00r9i4v6aiy6yabf22y27idl2cvjivdfwn21aypxr0d849rfg6q8";
    };
    installPhase = ''
      mkdir -p "$out"/share/kak/autoload/plugins/smartindent
      cp -r rc/*.kak "$out"/share/kak/autoload/plugins/smartindent
    '';
  };
in {
  programs.kakoune = {
    enable = true;
    extraConfig = builtins.readFile ./kakrc;
    plugins = with pkgs; with pkgs.kakounePlugins; [
      pkgs.kak-lsp
      kak-fzf
      kak-smartindent
    ];
  };
}

