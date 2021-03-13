{ nord-dircolors, ... }:

{ pkgs, ... }:

let
  toTOML = name: value: pkgs.runCommand name {
    buildInputs = [ pkgs.remarshal ];
    preferLocalBuild = true;
    allowSubstitutes = false;
  } ''
    remarshal -if json -of toml \
      < "${pkgs.writeText "${name}-json" (builtins.toJSON value)}" \
      > "$out"
  '';

  starship-config =
    let style = s: v: "[${v}](${s})";
    in toTOML "starship.toml" {
      git_status =
        let withCount = v: "${v}\${count}";
        in {
          format = "([\\[$ahead_behind$all_status\\]]($style) )";
          all_status = "$conflicted$stashed$deleted$renamed$modified$staged$untracked";
          ahead = style "bold bright-green" (withCount "⇡");
          behind = style "bold bright-green" (withCount "⇣");
          diverged = style "bold bright-green" ("⇡$ahead_count⇣$behind_count");
          stashed = style "bold bright-blue" (withCount "\\$");
          staged = style "bold yellow" (withCount "+");
          modified = style "bold yellow" (withCount "!");
          deleted = style "bold yellow" (withCount "✘");
          renamed = style "bold yellow" (withCount "»");
          untracked = style "bold bright-red" (withCount "?");
          conflicted = style "bold bright-red" (withCount "≠");
        };
      git_commit = {
        tag_disabled = false;
        tag_symbol = "";
      };
    };
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
      # direnv
      emulate zsh -c "$(${pkgs.direnv}/bin/direnv export zsh)"
      emulate zsh -c "$(${pkgs.direnv}/bin/direnv hook zsh)"

      # h
      eval "$(${pkgs.h}/bin/h --setup ~/code)"
      eval "$(${pkgs.h}/bin/up --setup)"

      # dircolors
      eval "$(${pkgs.coreutils}/bin/dircolors ${nord-dircolors}/src/dir_colors)"

      # starship
      export STARSHIP_CONFIG="${starship-config}"
      eval "$(${pkgs.starship}/bin/starship init zsh)"

      setopt autocd extendedglob
      unsetopt beep

      bindkey -v
      export KEYTIMEOUT=1

      # Edit current command line with "gi"
      bindkey -M vicmd gi edit-command-line

      export FZF_BASE="${pkgs.fzf}/share/fzf"

      # See github:spwhitt/nix-zsh-completions/issues/32
      function _nix() {
        local ifs_bk="$IFS"
        local input=("''${(Q)words[@]}")
        IFS=$'\n'$'\t'
        local res=($(NIX_GET_COMPLETIONS=$((CURRENT - 1)) "$input[@]"))
        IFS="$ifs_bk"
        local tpe="$res[1]"
        local suggestions=(''${res:1})
        if [[ "$tpe" == filenames ]]; then
          compadd -fa suggestions
        else
          compadd -a suggestions
        fi
      }
      compdef _nix nix
    '';

    shellAliases =
      let
        mkCdAlias = n: {
          name = ".${toString n}";
          value = "cd " + builtins.concatStringsSep "/" (builtins.genList (_: "..") n);
        };
        range = start: end: builtins.genList (x: x + start) (end - start + 1);
        # Alias ".2" to "cd ../..", ".3" to "cd ../../..", etc.
        # ".1"/".." is still needed even with zsh's autocd, since "cd .." works
        # even if your current directory no longer exists
        cdAliases = map mkCdAlias (range 1 9) ++ [{
          name = "..";
          value = "cd ..";
        }];
      in builtins.listToAttrs cdAliases // {
        ndone = "${pkgs.libnotify}/bin/notify-send 'Command finished'";

        # coreutils alternatives aliases
        ls = "${pkgs.exa}/bin/exa --git";
        cat = "${pkgs.bat}/bin/bat";

        # normalize path by resolving symlinks
        norm = ''cd "$(readlink -f .)"'';
      };

    ohMyZsh = {
      enable = true;
      plugins = [
        "vi-mode"
        "fzf"
        "git"
        "last-working-dir"
        "colored-man-pages"
        "command-not-found"
        "copybuffer"

        # completion plugins
        "cabal"
        "docker"
      ];
    };
  };

  environment = {
    systemPackages = with pkgs; [ direnv h ];
  };
}

