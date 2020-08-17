{ lib, ... }:

let
  sources = import ./sources.nix;
  utils = import ../common/utils.nix { inherit lib; };
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

    nixPath = [
      "nixpkgs=${sources.nixpkgs}"
      "nixos-config=/etc/nixos/configuration.nix"
      "/nix/var/nix/profiles/per-user/root/channels"
    ];
  };

  # Pin the system nixpkgs. Note that this causes nixpkgs.config and
  # nixpkgs.overlays to be ignored.
  nixpkgs.pkgs = import sources.nixpkgs {
    config = {
      allowUnfree = true;
      allowBroken = false;
      allowUnsupportedSystem = false;
    };

    # Import all the overlays in ./overlays
    overlays = builtins.map import
      (utils.getFilesWith (name: _: lib.hasSuffix ".nix" name) ./overlays);
  };
}
