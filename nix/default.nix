{ config, pkgs, ... }:

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

    packageOverrides = pkgs: rec {
      unstable = import ./unstable.nix { inherit pkgs config; };
    };
  };
}
