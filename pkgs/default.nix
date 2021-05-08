{ pkgs ? import <nixpkgs> {}
}:

{
  inherit (pkgs.callPackage ./development {})
    spim;

  inherit (pkgs.callPackage ./misc {})
    kakounePlugins
    zshPlugins;

  scripts = pkgs.callPackage ./scripts {};
}

