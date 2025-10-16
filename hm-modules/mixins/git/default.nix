{
  pkgs,
  config,
  lib,
  ...
}:
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
        squash = "commit --amend --no-edit";
        amend = "commit --amend";
        detach = "switch --detach";
        staged = "diff --cached";
        conflicts = "diff --name-status --diff-filter=U";
        ff = "merge --ff-only";
        ls = "log --oneline";
        compare = "log --left-right --graph --oneline";
        graph = "log --graph --abbrev-commit --date=relative --pretty=format:'%C(bold blue)%h - %C(reset)%C(green)(%ar)%C(reset) - %s %C(dim)- %an%C(reset)%C(yellow)%d'";
        changes = "show --name-status --oneline";
        mkexec = "update-index --chmod=+x";
        root = "rev-parse --show-toplevel";
        alias = ''! f(){ git config --get-regexp ^alias | cut -c 7- | sed -e "s/ \(.*\)/ = \1/"; }; f'';
        ignore = ''! f(){ curl -sL https://www.toptal.com/developers/gitignore/api/$@ ; }; f'';
        ignored = ''! f(){ find "$(realpath --relative-to=. "$(git rev-parse --show-toplevel)")" -type f -exec git check-ignore -v {} + | awk '{if ($1 !~ /^\//) print $2}' ; }; f'';
        praise = "blame";
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
