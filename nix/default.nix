{ config, pkgs, lib, ... }:

{
  system.autoUpgrade.enable = false;

  nix = {
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 30d";
    };

    trustedUsers = [
      "nprin"
      "root"
    ];
  };

  nixpkgs = {
    config = {
      allowUnfree = true;
      allowBroken = false;
      allowUnsupportedSystem = false;
    };

    # Import all the overlays in ./overlays
    overlays =
      let
        # Get all files in a directory
        getFiles = dir:
          builtins.map (x: dir + "/${x}")
          (builtins.attrNames
          (lib.filterAttrs (_: type: type == "regular" || type == "symlink")
          (builtins.readDir dir)));
      in builtins.map import (getFiles ./overlays);
  };
}
