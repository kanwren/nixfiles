{ pkgs, config }:

let
  unstableSrc = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz";
  };
in

import unstableSrc { config = config.nixpkgs.config; }
