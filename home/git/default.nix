{ pkgs, ... }:

{
  programs.git = {

    enable = true;
    package = pkgs.gitAndTools.gitFull;

    userName = "nprindle";
    userEmail = "nprindle18@gmail.com";

    ignores = [
      "tags"
    ];

    aliases = {
      # Aliases for filling in options
      cane = "commit --amend --no-edit";
      changed = "show --name-status --oneline";
      root = "rev-parse --show-toplevel";
      diffc = "diff --cached";
      mkexec = "update-index --chmod=+x";
      rh = "reset --hard";
      ri = "rebase --interactive";
      # `git log` aliases
      cmp = "log --left-right --graph --oneline";
      graph = "log --graph --oneline";
      ls = "log --oneline";
      # Abbreviations for common commands
      s = "status";
      co = "checkout";
      # Utility
      alias = ''! f(){ git config --get-regexp ^alias | cut -c 7- | sed -e "s/ \(.*\)/ = \1/"; }; f'';
      ignore-io = ''! f(){ join(){ local IFS=","; echo "$*"; }; curl -sL https://www.toptal.com/developers/gitignore/api/$(join $*); }; f'';
      # Jokes
      praise = "blame";
      pansect = "bisect";
    };

    extraConfig = {
      gist = {
        private = true;
      };
      credential = {
        helper = "manager";
      };
      core = {
        editor = "nvim";
        autocrlf = false;
      };
      diff = {
        tool = "vimdiff";
      };
      difftool = {
        prompt = false;
      };
      merge = {
        tool = "vimdiff";
        conflictstyle = "diff3";
      };
      mergetool = {
        prompt = false;
        keepBackup = false;
      };
      push = {
        default = "simple";
      };
      pull = {
        ff = "only";
      };
      advice = {
        addEmptyPathspec = false;
      };
    };

    lfs.enable = true;

    signing = {
      key = "nprindle18@gmail.com";
      signByDefault = true;
    };
  };

  programs.bash.sessionVariables = {
    GPGKEY = "nprindle18@gmail.com";
  };
}

