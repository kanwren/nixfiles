{ config, pkgs, ... }:

{
  home-manager.users.nprin = {
    programs.git = {

      enable = true;
      package = pkgs.gitAndTools.gitFull;

      userName = "nprindle";
      userEmail = "nprindle18@gmail.com";

      ignores = [
        "tags"
      ];

      aliases = {
        # Abbreviations for filling in options
        cane = "commit --amend --no-edit";
        changed = "show --name-status --oneline";
        cmp = "log --left-right --graph --oneline";
        graph = "log --graph --oneline";
        ls = "log --oneline";
        rh = "reset --hard";
        root = "rev-parse --show-toplevel";
        diffc = "diff --cached";
        exec = "update-index --chmod=+x";
        # Abbreviations for common commands
        s = "status";
        co = "checkout";
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
          editor = "vim";
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
      };

      lfs.enable = true;

    };
  };
}

