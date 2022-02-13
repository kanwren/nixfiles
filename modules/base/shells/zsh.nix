{ pkgs, self, system, ... }:

let
  toml = pkgs.formats.toml { };
  starship-config =
    let style = s: v: "[${v}](${s})";
    in
    toml.generate "starship.toml" {
      git_status =
        let withCount = v: "${v}\${count}";
        in
        {
          format = "([\\[$ahead_behind$conflicted$stashed$deleted$renamed$modified$staged$untracked\\]]($style) )";
          # Note: all_status doesn't work anymore, inlined above
          # all_status = "$conflicted$stashed$deleted$renamed$modified$staged$untracked";
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
in
{
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

    histSize = 100000;

    interactiveShellInit = ''
      export HISTORY_IGNORE='([bf]g *|cd( *)#|.[.123456789]|l[alsh]#( *)#|less *|(nvim|vim#)( *)#)'

      # direnv
      emulate zsh -c "$(${pkgs.direnv}/bin/direnv export zsh)"
      emulate zsh -c "$(${pkgs.direnv}/bin/direnv hook zsh)"

      # h
      eval "$(${pkgs.h}/bin/h --setup ~/code)"
      eval "$(${pkgs.h}/bin/up --setup)"

      # starship
      export STARSHIP_CONFIG="${starship-config}"
      eval "$(${pkgs.starship}/bin/starship init zsh)"

      setopt autocd extendedglob
      unsetopt beep

      bindkey -v
      export KEYTIMEOUT=1

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

      # Fix fzf not being loaded with zsh-vi-mode
      # See https://github.com/jeffreytse/zsh-vi-mode#execute-extra-commands
      function zvm_after_init() {
        source "${pkgs.oh-my-zsh}/share/oh-my-zsh/plugins/fzf/fzf.plugin.zsh"
      }
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
      in
      builtins.listToAttrs cdAliases // {
        # aliases for alternate utilities
        ls = "${pkgs.exa}/bin/exa --git";
        cat = "${pkgs.bat}/bin/bat";
        ping = "${pkgs.prettyping}/bin/prettyping";

        # normalize path by resolving symlinks
        norm = ''cd "$(readlink -f .)"'';
      };

    ohMyZsh = {
      enable = true;
      plugins = [
        # shell interaction
        "zsh-vi-mode" # "vi-mode"
        "safe-paste" # prevent pasted code from running before review
        "last-working-dir" # switches new shells to last working dir, adds 'lwd'
        "command-not-found" # shows packages that provide missing commands
        "nix-shell" # lets nix-shell use zsh
        "colored-man-pages" # colors man pages

        "fzf" # finds fzf and enables completions, etc.
        "git" # provides lots of git aliases

        # navigation
        "z" # adds 'z' to jump to directory matching pattern based on frecency
        "wd" # bookmark directories for quick jumping, adds 'wd'

        # utility
        "cp" # adds 'cpv' which uses rsync
        "copyfile" # adds 'copyfile' to copy file contents to the clipboard
        "copydir" # adds 'copydir' to copy the CWD
        "universalarchive" # adds 'ua' for easy archiving into different formats

        # completion plugins
        "ripgrep"
        "cabal"
        "rust"
        "docker"
      ];
      customPkgs = [
        self.packages.${system}."zshPlugins/zsh-vi-mode"
        (pkgs.runCommand "zsh-nix-shell" { } ''
          mkdir -p "$out"/share/zsh/plugins
          cp -r ${pkgs.zsh-nix-shell}/share/zsh-nix-shell "$out"/share/zsh/plugins/nix-shell
        '')
      ];
    };
  };

  programs.command-not-found.enable = true;

  environment = {
    systemPackages = with pkgs; [ direnv h ];
  };
}

