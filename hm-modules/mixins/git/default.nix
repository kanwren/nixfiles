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
        ".claude/"
        "CLAUDE.local.md"
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
        credential = {
          helper = "manager";
        };
        gist = {
          private = true;
        };
        core = {
          editor = "nvim";
          autocrlf = false;
        };
        color = {
          diff = "auto";
          status = "auto";
          branch = "auto";
          interactive = "auto";
        };
        column = {
          ui = "auto";
        };
        log = {
          mailmap = true;
        };
        init = {
          defaultBranch = "main";
        };
        branch = {
          sort = "-committerdate";
          autosetupmerge = true;
        };
        tag = {
          sort = "version:refname";
        };
        filter.lfs = {
          clean = "git-lfs clean -- %f";
          smudge = "git-lfs smudge -- %f";
          process = "git-lfs filter-process";
          required = true;
        };
        rerere = {
          enabled = true;
          autoupdate = true;
        };
        commit = {
          verbose = true;
        };
        fetch = {
          prune = true;
          pruneTags = true;
          all = true;
        };
        pull = {
          ff = "only";
        };
        push = {
          default = "simple";
          autoSetupRemote = true;
          followTags = true;
        };
        diff = {
          algorithm = "histogram";
          tool = "nvimdiff";
          renames = true;
          indentHeuristic = "on";
          colorMoved = "plain";
          mnemonicPrefix = true;
        };
        difftool = {
          prompt = false;
        };
        rebase = {
          autoSquash = true;
          autoStash = true;
          updateRefs = true;
        };
        merge = {
          summary = true;
          tool = "nvimdiff";
          conflictstyle = "zdiff3";
        };
        mergetool = {
          prompt = false;
          keepBackup = false;
        };
        advice = {
          addEmptyPathspec = false;
        };
      };

      lfs.enable = true;

      signing = {
        key = "002937658A2F43138C3B267E339C3A5C672CEA46";
        signByDefault = true;
      };
    };

    home.sessionVariables = {
      GPGKEY = "nicole@wren.systems";
    };
  };
}

