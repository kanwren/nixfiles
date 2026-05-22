{
  flake.modules = {
    nixos.shell = {
      programs.bash.enable = true;
    };

    darwin.shell = {
      programs.bash.enable = true;
    };

    homeManager.shell = {
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
        historyControl = [
          "ignorespace"
          "ignoredups"
        ];
        historyIgnore = [
          "ls"
          "cd"
          "exit"
          "history"
        ];

        initExtra = ''
          test "$(ulimit -n)" -lt 8192 && ulimit -n 8192
          set -o vi
          PS='; '
          bind -m vi-insert "\C-l":clear-screen
        '';
      };
    };
  };
}
