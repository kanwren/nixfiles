{ pkgs ? import <nixpkgs> { }
, nur
}:

let
  inherit (pkgs) lib;
in
{
  # Re-export NUR as a package set
  nur = import nur { nurpkgs = pkgs; inherit pkgs; };

  inherit (pkgs.callPackage ./development { })
    spim;

  inherit (pkgs.callPackage ./misc { })
    kakounePlugins
    zshPlugins;

  scripts = lib.recurseIntoAttrs (pkgs.callPackage ./scripts { });
}

