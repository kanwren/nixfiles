{ ... }:

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

    historyFile = "$HOME/.bash_history";
    historySize = 1000000;
    historyFileSize = 1000000;
    historyControl = [ "ignorespace" "ignoredups" ];
    historyIgnore = [ "ls" "cd" "exit" "history" ];

    # User-specific shell aliases
    shellAliases = { };

    initExtra = ''
      test "$(ulimit -n)" -lt 8192 && ulimit -n 8192
      set -o vi
      PS='; '
      bind -m vi-insert "\C-l":clear-screen
    '';
  };
}
