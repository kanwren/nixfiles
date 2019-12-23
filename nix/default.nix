{ config, pkgs, ... }:

let
  inherit (config.lib) utils;
in
{
  imports = [
    ./caches.nix
  ];

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
    overlays = builtins.map import (utils.getFiles ./overlays);
  };
}
