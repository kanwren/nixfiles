{ pkgs, lib, ... }:

{
  environment.shellAliases = lib.mergeAttrsList [
    {
      ls = "${pkgs.eza}/bin/eza --git";
      cat = "${pkgs.bat}/bin/bat";
      ping = "${pkgs.prettyping}/bin/prettyping";
      xy = "xclip -selection clip -in";
      xp = "xclip -selection clip -out";
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
    fish.enable = true;
  };
}

