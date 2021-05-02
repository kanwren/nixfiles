{ pkgs ? import <nixpkgs> {}
}:

{
  inherit (pkgs.callPackage ./misc {})
    kakounePlugins
    zshPlugins;
}
