{ neovim }:

{ lib, pkgs, ... }:

{
  imports = [
    ./caches.nix
    (import ./overlays { inherit neovim; })
  ];

  system.autoUpgrade.enable = false;

  nix = {
    package = pkgs.nixFlakes;

    extraOptions = ''
      experimental-features = nix-command flakes
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
