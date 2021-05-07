{ pkgs ? import <nixpkgs> {}
}:

pkgs.lib.foldl' (pkgs.lib.mergeAttrsNoOverride {}) {} [
  (pkgs.callPackage ./development {})
  (pkgs.callPackage ./misc {})
]

