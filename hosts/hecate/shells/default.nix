{ pkgs, lib, flake, ... }:

let
  cdAliases = builtins.listToAttrs (builtins.map
    (n: {
      name = ".${toString n}";
      value = "cd ${builtins.concatStringsSep "/" (builtins.genList (_: "..") n)}";
    })
    (lib.lists.range 1 9));
in

{
  programs = {
    command-not-found.enable = false;

    bash = {
      enableCompletion = true;

      shellAliases = cdAliases // (with pkgs; {
        ls = "${coreutils}/bin/ls --color=auto";
        cat = "${pkgs.bat}/bin/bat";
        ping = "${pkgs.prettyping}/bin/prettyping";
      });

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
      '';

      promptInit = ''
        PS1='; '
      '';
    };

    fish = {
      enable = true;
      shellAliases = cdAliases // {
        ls = "${pkgs.eza}/bin/eza --git";
        cat = "${pkgs.bat}/bin/bat";
        ping = "${pkgs.prettyping}/bin/prettyping";
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
    flake.packages.${pkgs.system}.wd-fish
  ];
}

