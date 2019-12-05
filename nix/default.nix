{ config, pkgs, ... }:

let
  unstablePkgs = import ./unstable.nix {
    nixpkgsConfig = config.nixpkgs.config;
  };
in
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

  nixpkgs.config = {
    allowUnfree = true;
    allowBroken = false;
    allowUnsupportedSystem = false;

    packageOverrides = pkgs: {
      inherit unstablePkgs;
    };
  };
}
