{ pkgs, lib, ... }:

{
  environment.shellAliases = lib.mergeAttrsList [
    {
      l = "${pkgs.eza}/bin/eza --git";
      ping = "${pkgs.prettyping}/bin/prettyping";
      show = "${pkgs.bat}/bin/bat";
    }
    (builtins.listToAttrs
      (builtins.map
        (n: {
          name = ".${toString n}";
          value = "cd ${builtins.concatStringsSep "/" (builtins.genList (_: "..") n)}";
        })
        (lib.lists.range 1 9)))
  ];

  programs = {
    command-not-found.enable = false;
    fish = {
      enable = true;
      shellAbbrs = {
        xy = "xsel -ib";
        xp = "xsel -ob";
      };
    };
  };
}

