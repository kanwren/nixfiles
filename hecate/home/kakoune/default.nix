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
  kak-readline = pkgs.stdenv.mkDerivation {
    name = "kak-readline";
    src = pkgs.fetchFromGitHub {
      owner = "chambln";
      repo = "kakoune-readline";
      rev = "8029c0eee75d41401184c06620bf0f45240d9a14";
      sha256 = "180s960g6y7gqjj20i4k4ihp6ajyf3b0bbhchqrlxvqzpaxxbrd5";
    };
    installPhase = ''
      mkdir -p "$out"/share/kak/autoload/plugins/readline
      cp -r readline.kak "$out"/share/kak/autoload/plugins/readline
    '';
  };
  kak-mirror = pkgs.stdenv.mkDerivation {
    name = "kak-mirror";
    src = pkgs.fetchFromGitHub {
      owner = "Delapouite";
      repo = "kakoune-mirror";
      rev = "5710635f440bcca914d55ff2ec1bfcba9efe0f15";
      sha256 = "0fd65clx9p6sslrl3l25m8l9ihl2mqrvvmmkjqr3bgk16vip3jds";
    };
    installPhase = ''
      mkdir -p "$out"/share/kak/autoload/plugins/mirror
      cp -r mirror.kak "$out"/share/kak/autoload/plugins/mirror
    '';
  };
in {
  programs.kakoune = {
    enable = true;
    extraConfig = builtins.readFile ./kakrc;
    plugins = with pkgs; [
      kak-lsp
      kakounePlugins.kak-fzf
      kakounePlugins.case-kak
      kak-smartindent
      kak-readline
      kak-mirror
    ];
  };
}

