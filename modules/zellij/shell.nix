{
  flake.modules.homeManager.zellij = {
    programs.fish.shellAbbrs =
      let
        commandAbbr = cmd: {
          position = "command";
          expansion = cmd;
        };
      in
      {
        "zj" = commandAbbr "zellij";
        "zjr" = commandAbbr "zellij run --";
        "zje" = commandAbbr "zellij edit";
        "zjl" = commandAbbr "zellij list-sessions";
        "zja" = commandAbbr "zellij attach";
        "zjk" = commandAbbr "zellij kill-session";
        "zjd" = commandAbbr "zellij delete-all-sessions";
      };
  };
}
