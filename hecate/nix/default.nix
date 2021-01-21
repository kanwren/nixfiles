deps:

{ lib, pkgs, ... }:

{
  imports = [
    ./caches.nix
    (import ./overlays deps)
  ];

  system.autoUpgrade.enable = false;

  nix = {
    extraOptions = ''
      keep-outputs = true
      keep-derivations = true
    '';

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

  nixpkgs.config.allowUnfree = true;
}
