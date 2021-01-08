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
      cm = "commit --message";
      amend = "commit --amend";

      cob = "checkout -b";
      detach = "checkout --detach";

      diffc = "diff --cached";
      conflicts = "diff --name-status --diff-filter=U";

      rh = "reset --hard";

      ri = "rebase --interactive";

      ls = "log --oneline";
      graph = "log --graph --oneline";
      cmp = "log --left-right --graph --oneline";

      changed = "show --name-status --oneline";

      mkexec = "update-index --chmod=+x";

      root = "rev-parse --show-toplevel";

      # Abbreviations for common commands
      s = "status";
      b = "branch";
      co = "checkout";
      r = "reset";

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

    # signing = {
    #   key = "nprindle18@gmail.com";
    #   signByDefault = true;
    # };
  };

  # home.sessionVariables = {
  #   GPGKEY = "nprindle18@gmail.com";
  # };
}

