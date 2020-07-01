{ pkgs, ... }:

let
  ls_colors = pkgs.fetchFromGitHub {
    owner = "arcticicestudio";
    repo = "nord-dircolors";
    rev = "addb3b427e008d23affc721450fde86f27566f1d";
    sha256 = "0s7bd38269z4b9j6f90nscjkbdbh23z3mlg89fnk7ndyrpf5dqlj";
  } + "/src/dir_colors";
in {
  programs.zsh = {
    enable = true;
    enableCompletion = true;

    autosuggestions = {
      enable = true;
    };

    syntaxHighlighting = {
      enable = true;
      highlighters = [ "main" "brackets" "cursor" ];
    };

    interactiveShellInit = ''
      eval "$(${pkgs.h}/bin/h --setup ~/code)"
      eval "$(${pkgs.h}/bin/up --setup)"

      eval "$(${pkgs.coreutils}/bin/dircolors ${ls_colors})"

      setopt autocd extendedglob
      unsetopt beep
      bindkey -v
    '';

    ohMyZsh = {
      enable = true;
      theme = "bira";
      plugins = [
        "vi-mode"
        "direnv"
        "git-prompt"
        "colored-man-pages"
        "cabal"
        "docker"
      ];
    };
  };

  environment = {
    systemPackages = with pkgs; [ direnv h ];
  };
}

