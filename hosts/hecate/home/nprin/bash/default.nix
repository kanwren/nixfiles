{ pkgs, ... }:

# Many of the settings here intentionally mirror the global interactive shell
# init settings
{
  programs.bash = {
    enable = true;

    shellOptions = [
      "checkwinsize"
      "extglob"
      "globstar"
      "checkjobs"
      "histappend"
      "cmdhist"
    ];

    historyControl = [ "ignorespace" "ignoredups" ];
    historySize = 1000;
    historyFileSize = 2000;
    historyIgnore = [ "ls" "cd" "exit" "history" ];

    # User-specific shell aliases
    shellAliases = with pkgs; { };

    initExtra = ''
      # Set up 'h'
      eval "$(${pkgs.h}/bin/h --setup ~/code)"
      eval "$(${pkgs.h}/bin/up --setup)"

      set -o vi
      bind -m vi-insert "\C-l":clear-screen
    '';
  };
}
