{ pkgs, ... }:

let
  # Better ls colors than the default, call dircolors on it and eval
  ls_colors = pkgs.fetchFromGitHub {
    owner = "trapd00r";
    repo = "LS_COLORS";
    rev = "8256268e688d23892cab1785a8b0ea6e4353c3f4";
    sha256 = "1zrb4niz26mk42s88d0az2r1j1cjn509fbg2p7iny6p0a0b3mp02";
  } + "/LS_COLORS";
in rec {
  programs.bash = {
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
    };
  };

  environment = {
    systemPackages = with pkgs; [ bashInteractive direnv ];

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

      # Enable direnv
      eval "$(${pkgs.direnv}/bin/direnv hook bash)"

      # Add some better ls colors
      eval "$(${pkgs.coreutils}/bin/dircolors ${ls_colors})"
    '';
  };

}

