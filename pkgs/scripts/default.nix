{ pkgs
}:

let
  inherit (pkgs) lib;
  haskellScript = pkgs.callPackage ./haskell-script.nix { };
  addmeta = p: meta: p.overrideAttrs (old: {
    meta = (old.meta or { }) // meta;
  });
in

{
  # util
  bak = pkgs.callPackage ./util/bak { inherit addmeta; };
  random = pkgs.callPackage ./util/random { inherit haskellScript addmeta; };
  serve = pkgs.callPackage ./util/serve { inherit haskellScript addmeta; };
  truthtable = pkgs.callPackage ./util/truthtable { inherit haskellScript addmeta; };

  # system
  nosleep = pkgs.callPackage ./system/nosleep { inherit addmeta; };
  toggle = pkgs.callPackage ./system/toggle { inherit addmeta; };

  firefox-app = pkgs.callPackage ./firefox-app { inherit addmeta; };

  # nix
  nix-gcroots = pkgs.callPackage ./nix/nix-gcroots { inherit addmeta; };
  comma = pkgs.callPackage ./nix/comma { inherit addmeta; };
  add-rpath = pkgs.callPackage ./nix/add-rpath { inherit addmeta; };

  # cs2110
  autograde = pkgs.callPackage ./cs2110/autograde { };
  csutils = lib.recurseIntoAttrs (pkgs.callPackage ./cs2110/csutils { inherit addmeta; });
}

