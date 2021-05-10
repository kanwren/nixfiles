{ pkgs ? import <nixpkgs> {}
}:

let
  inherit (pkgs) lib;
in
{
  inherit (pkgs.callPackage ./development {})
    spim;

  inherit (pkgs.callPackage ./misc {})
    kakounePlugins
    zshPlugins;

  scripts = lib.recurseIntoAttrs (pkgs.callPackage ./scripts {});
}

