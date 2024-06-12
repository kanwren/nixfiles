{ pkgs, config, lib, ... }:

let
  cfg = config.mixins.git;
in
{
  options.mixins.git.enable = lib.mkOption {
    type = lib.types.bool;
    default = config.mixins.enable;
    description = "Whether to enable the git mixin";
  };

  config = lib.mkIf cfg.enable {
    programs.git = {

      enable = true;
      package = pkgs.gitAndTools.gitFull;

      userName = "Nicole Wren";
      userEmail = "nicole@wren.systems";

      ignores = [
        "tags"
        ".direnv/"
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
        # diff with filter
        difff = "diff --diff-filter";
        # diff file names only; for example, "git diffno --diff-filter=U | xargs vim"
        diffno = "diff --name-only";
        diffnof = "diff --name-only --diff-filter";

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
        gist.private = true;
        credential.helper = "manager";
        core = {
          editor = "nvim";
          autocrlf = false;
        };
        init.defaultBranch = "main";
        diff.tool = "vimdiff";
        difftool.prompt = false;
        merge = {
          tool = "vimdiff";
          conflictstyle = "diff3";
        };
        mergetool = {
          prompt = false;
          keepBackup = false;
        };
        push.default = "simple";
        pull.ff = "only";
        advice.addEmptyPathspec = false;
      };

      lfs.enable = true;

      signing = {
        key = "nicole@wren.systems";
        signByDefault = true;
      };
    };

    home.sessionVariables = {
      GPGKEY = "nicole@wren.systems";
    };
  };
}

