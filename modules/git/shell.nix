{
  flake.modules.homeManager.git = {
    programs.fish.shellAbbrs =
      let
        commandAbbr = cmd: {
          position = "command";
          expansion = cmd;
        };
      in
      {
        "g" = commandAbbr "git";
        "ga" = commandAbbr "git add";
        "gb" = commandAbbr "git branch";
        "gc" = commandAbbr "git commit";
        "gd" = commandAbbr "git diff";
        "gl" = commandAbbr "git log";
        "gp" = commandAbbr "git pull";
        "gpf" = commandAbbr "git push --force-with-lease";
        "gr" = commandAbbr "git rebase";
        "gra" = commandAbbr "git rebase --abort";
        "grc" = commandAbbr "git rebase --continue";
        "gro" = commandAbbr "git rebase --onto";
        "gri" = commandAbbr "git rebase --interactive";
        "grs" = commandAbbr "git restore";
        "gsh" = commandAbbr "git show";
        "gst" = commandAbbr "git status";
        "gsw" = commandAbbr "git switch";
        "gswc" = commandAbbr "git switch --create";
        "gswd" = commandAbbr "git switch --detach";
        "gx" = commandAbbr "git reset";
        "gxm" = commandAbbr "git reset --mixed";
        "gxh" = commandAbbr "git reset --hard";
        "gxs" = commandAbbr "git reset --soft";
      };
  };
}
