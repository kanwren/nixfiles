{
  flake.modules.homeManager.git =
    { pkgs, ... }:
    {
      programs.git = {
        enable = true;
        package = pkgs.gitFull;

        settings = {
          alias = {
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
            ignore = "! f(){ curl -sL https://www.toptal.com/developers/gitignore/api/$@ ; }; f";
            ignored = ''! f(){ find "$(realpath --relative-to=. "$(git rev-parse --show-toplevel)")" -type f -exec git check-ignore -v {} + | awk '{if ($1 !~ /^\//) print $2}' ; }; f'';
            praise = "blame";
          };

          credential.helper = if pkgs.stdenv.hostPlatform.isDarwin then "osxkeychain" else "manager";

          gist.private = true;

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

          column.ui = "auto";

          log.mailmap = true;

          init.defaultBranch = "main";

          branch = {
            sort = "-committerdate";
            autosetupmerge = true;
          };

          tag.sort = "version:refname";

          filter.lfs.required = true;

          rerere = {
            enabled = true;
            autoupdate = true;
          };

          commit.verbose = true;

          fetch = {
            prune = true;
            pruneTags = true;
            all = true;
          };

          pull.ff = "only";

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

          difftool.prompt = false;

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

          advice.addEmptyPathspec = false;
        };

        ignores = [
          "*.iml"
          "*.swo"
          "*.swp"
          ".DS_Store"
          ".bundle"
          ".claude/"
          ".direnv/"
          ".idea"
          ".jj"
          ".rbx"
          "/tags"
          "CLAUDE.local.md"
          "node_modules"
          "tags"
        ];

        lfs.enable = true;
      };
    };

}
