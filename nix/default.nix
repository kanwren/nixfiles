{ lib, ... }:

let
  utils = import ../utils { inherit lib; };
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

    extraOptions = ''
      keep-outputs = true
      keep-derivations = true
    '';
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
