{ pkgs, ... }:

let
  nord_dircolors = pkgs.fetchFromGitHub {
    owner = "arcticicestudio";
    repo = "nord-dircolors";
    rev = "addb3b427e008d23affc721450fde86f27566f1d";
    sha256 = "0s7bd38269z4b9j6f90nscjkbdbh23z3mlg89fnk7ndyrpf5dqlj";
  } + "/src/dir_colors";
  # Wrap nixpkgs.zsh-powerlevel10k so oh-my-zsh can find it
  powerlevel10k-omz = pkgs.runCommand "link-zsh-powerlevel10k" {} ''
    mkdir -p "$out/share/zsh/themes"
    ln -s ${pkgs.zsh-powerlevel10k}/share/zsh-powerlevel10k "$out/share/zsh/themes/powerlevel10k"
  '';
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
      # Enable powerlevel10k instant prompt
      if [[ -r "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh" ]]; then
        source "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh"
      fi

      eval "$(${pkgs.h}/bin/h --setup ~/code)"
      eval "$(${pkgs.h}/bin/up --setup)"

      eval "$(${pkgs.coreutils}/bin/dircolors ${nord_dircolors})"

      setopt autocd extendedglob
      unsetopt beep

      bindkey -v
      export KEYTIMEOUT=1
    '' + builtins.readFile ./p10k.zsh;

    shellAliases =
      let
        mkCdAlias = n: {
          name = ".${toString n}";
          value = "cd " + builtins.concatStringsSep "/" (builtins.genList (_: "..") n);
        };
        range = start: end: builtins.genList (x: x + start) (end - start + 1);
        # Alias ".2" to "cd ../..", ".3" to "cd ../../..", etc.
        cdAliases = builtins.listToAttrs (map mkCdAlias (range 2 9));
      in cdAliases // {
        nrn = "${pkgs.nix}/bin/nix repl '<nixpkgs>'";
        nrnn = "${pkgs.nix}/bin/nix repl '<nixpkgs>' '<nixpkgs/nixos>'";
      };

    ohMyZsh = {
      enable = true;
      theme = "powerlevel10k/powerlevel10k";
      plugins = [
        "vi-mode"
        "direnv"
        "fzf"
        "git-prompt"
        "last-working-dir"
        "colored-man-pages"

        # completion plugins
        "cabal"
        "docker"
      ];
      customPkgs = [ powerlevel10k-omz ];
    };
  };

  environment = {
    systemPackages = with pkgs; [ direnv h ];
  };
}

