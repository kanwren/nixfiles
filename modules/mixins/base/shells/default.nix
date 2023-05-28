{ self }:

{ pkgs, lib, ... }:

let
  cdAliases = builtins.listToAttrs (builtins.map
    (n: {
      name = ".${toString n}";
      value = "cd ${builtins.concatStringsSep "/" (builtins.genList (_: "..") n)}";
    })
    (lib.lists.range 1 9));
in

{
  imports = [
    ../starship.nix
  ];

  programs = {
    command-not-found.enable = false;

    zsh = {
      enable = true;
      enableCompletion = true;
      autosuggestions.enable = true;

      syntaxHighlighting = {
        enable = true;
        highlighters = [ "main" "brackets" "cursor" ];
      };

      histSize = 1000000;

      interactiveShellInit = ''
        export HISTORY_IGNORE='([bf]g *|cd( *)#|.[.123456789]|l[alsh]#( *)#|less *|(nvim|vim#)( *)#)'

        # direnv
        emulate zsh -c "$(${pkgs.direnv}/bin/direnv export zsh)"
        emulate zsh -c "$(${pkgs.direnv}/bin/direnv hook zsh)"

        # h
        eval "$(${pkgs.h}/bin/h --setup ~/code)"
        eval "$(${pkgs.h}/bin/up --setup)"

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

        function nix-explore() {
          if ! nix build "$1" --no-link -v; then
            return $?
          fi

          if ! _nix_explore_path_info="$(nix path-info "$1")"; then
            return $?
          fi

          if [ $(echo "$_nix_explore_path_info" | wc -l) -eq 1 ]; then
            cd "$_nix_explore_path_info"
          else
            >&2 echo "Error: couldn't get unique path info"
            >&2 echo "$_nix_explore_path_info"
          fi
        }
      '';

      shellAliases = cdAliases // {
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
          "nix-shell" # lets nix-shell use zsh
          "colored-man-pages" # colors man pages

          "fzf" # finds fzf and enables completions, etc.
          "git" # provides lots of git aliases

          # navigation
          "z" # adds 'z' to jump to directory matching pattern based on frecency
          "wd" # bookmark directories for quick jumping, adds 'wd'

          # utility
          "copyfile" # adds 'copyfile' to copy file contents to the clipboard
          "copypath"
          "universalarchive" # adds 'ua' for easy archiving into different formats

          # completion plugins
          "ripgrep"
          "cabal"
          "rust"
          "docker"
          "bazel"
        ];
        customPkgs = [
          (pkgs.runCommand "zsh-vi-mode" { } ''
            mkdir -p "$out"/share/zsh/plugins
            cp -r ${pkgs.zsh-vi-mode}/share/zsh-vi-mode "$out"/share/zsh/plugins/zsh-vi-mode
          '')
          (pkgs.runCommand "zsh-nix-shell" { } ''
            mkdir -p "$out"/share/zsh/plugins
            cp -r ${pkgs.zsh-nix-shell}/share/zsh-nix-shell "$out"/share/zsh/plugins/nix-shell
          '')
        ];
      };
    };

    bash = {
      enableCompletion = true;

      shellAliases = with pkgs; {
        ls = "${coreutils}/bin/ls --color=auto";
        grep = "${gnugrep}/bin/grep --color=auto";
        fgrep = "${gnugrep}/bin/fgrep -F --color=auto";
        egrep = "${gnugrep}/bin/egrep -E --color=auto";
        diff = "${colordiff}/bin/colordiff";

        ".." = "cd ..";
        ".1" = "cd ..";
        ".2" = "cd ../..";
        ".3" = "cd ../../..";
        ".4" = "cd ../../../..";
        ".5" = "cd ../../../../..";
        ".6" = "cd ../../../../../..";
        ".7" = "cd ../../../../../../..";
        ".8" = "cd ../../../../../../../..";
        ".9" = "cd ../../../../../../../../..";

        norm = ''cd "$(readlink -f .)"'';
      };

      interactiveShellInit = ''
        set -o vi
        # Make C-l clear the screen in insert mode
        bind -m vi-insert "\C-l":clear-screen

        # Check update LINES and COLUMNS after each command based on window size
        shopt -s checkwinsize
        # Extend the globbing behavior
        shopt -s extglob
        # Use ** to match zero or more subdirectories
        shopt -s globstar
        # Check before closing terminal
        shopt -s checkjobs
        # Append to the history file, don't overwrite it
        shopt -s histappend
        # Store multiline commands in one history entry
        shopt -s cmdhist

        # Don't put duplicate lines or lines starting with space in the history.
        HISTCONTROL=ignoreboth
        HISTSIZE=1000
        HISTFILESIZE=2000
        HISTIGNORE="ls:cd:exit:history"

        # Set colorful prompt
        case "$TERM" in
          *color|alacritty|xterm-kitty) color_prompt=true ;;
        esac

        if [ "$color_prompt" ]; then
          PS1='\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
        else
          PS1='\u@\h:\w\$ '
        fi

        function nix-explore() {
          if ! nix build "$1" --no-link -v; then
            return $?
          fi

          if ! _nix_explore_path_info="$(nix path-info "$1")"; then
            return $?
          fi

          if [ $(echo "$_nix_explore_path_info" | wc -l) -eq 1 ]; then
            cd "$_nix_explore_path_info"
          else
            >&2 echo "Error: couldn't get unique path info"
            >&2 echo "$_nix_explore_path_info"
          fi
        }
      '';
    };

    fish = {
      enable = true;
      shellAliases = cdAliases // {
        ls = "${pkgs.exa}/bin/exa --git";
        cat = "${pkgs.bat}/bin/bat";
        ping = "${pkgs.prettyping}/bin/prettyping";
        g = "git";
      };
      interactiveShellInit = ''
        direnv hook fish | source

        function h
            if set -l target (command h --resolve "$HOME/code" $argv)
                if [ $target != (pwd) ]
                    echo $target
                    cd $target
                end
            else
                return $status
            end
        end

        function up
            if set -l target (command up $argv)
                if [ $target != (pwd) ]
                    echo $target
                    cd $target
                end
            else
                return $status
            end
        end
      '';
    };
  };

  environment.systemPackages = with pkgs; [
    direnv
    h
    fishPlugins.fzf-fish
    fishPlugins.foreign-env
    self.packages.${pkgs.system}.wd-fish
  ];
}

