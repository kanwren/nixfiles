{ nord-dircolors, ... }:

{ pkgs, ... }:

let
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
      emulate zsh -c "$(${pkgs.direnv}/bin/direnv export zsh)"

      # Enable powerlevel10k instant prompt
      if [[ -r "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh" ]]; then
        source "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh"
      fi

      emulate zsh -c "$(${pkgs.direnv}/bin/direnv hook zsh)"

      eval "$(${pkgs.h}/bin/h --setup ~/code)"
      eval "$(${pkgs.h}/bin/up --setup)"

      eval "$(${pkgs.coreutils}/bin/dircolors ${nord-dircolors}/src/dir_colors)"

      setopt autocd extendedglob
      unsetopt beep

      bindkey -v
      export KEYTIMEOUT=1

      # Edit current command line with "gi"
      bindkey -M vicmd gi edit-command-line

      export FZF_BASE="${pkgs.fzf}/share/fzf"
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
        ndone = "${pkgs.libnotify}/bin/notify-send 'Command finished'";

        # coreutils alternatives aliases
        ls = "${pkgs.exa}/bin/exa --git";
        cat = "${pkgs.bat}/bin/bat";
      };

    ohMyZsh = {
      enable = true;
      theme = "powerlevel10k/powerlevel10k";
      plugins = [
        "vi-mode"
        "fzf"
        "git-prompt"
        "git"
        "last-working-dir"
        "colored-man-pages"
        "command-not-found"
        "copybuffer"

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

