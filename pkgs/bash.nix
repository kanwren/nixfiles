{ config, pkgs, ... }:

rec {
  environment = {
    systemPackages = with pkgs; [ bash ];

    interactiveShellInit = programs.bash.interactiveShellInit;
  };

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

      cleantex = "rm *.aux *.log";
      copylast = "fc -ln -1 | ${gawk}/bin/awk '{\$1=\$1}1' | ${xclip}/bin/xclip -sel clip";
      xc = "${xclip}/bin/xclip -sel clip";
      getpass = "${openssl}/bin/openssl rand -base64";

      nrn = "${nix}/bin/nix repl '<nixpkgs>' '<nixpkgs/nixos>'";
    };

    interactiveShellInit = ''
      set -o vi
      # Make C-l clear the screen in insert mode
      bind -m vi-insert "\C-l":clear-screen

      # Check update LINES and COLUMNS after each command based on window size
      shopt -s checkwinsize
      # Use ** to match zero or more subdirectories
      shopt -s globstar

      # Don't put duplicate lines or lines starting with space in the history.
      HISTCONTROL=ignoreboth
      HISTSIZE=1000
      HISTFILESIZE=2000
      # Append to the history file, don't overwrite it
      shopt -s histappend

      # Set colorful prompt
      case "$TERM" in
        xterm-color|*-256color|*-256-color|alacritty) color_prompt=true ;;
      esac

      if [ "$color_prompt" ]; then
        PS1='\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
      else
        PS1='\u@\h:\w\$ '
      fi
    '';
  };

}

