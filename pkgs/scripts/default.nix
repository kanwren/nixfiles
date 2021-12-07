{ pkgs
}:

let
  inherit (pkgs) lib;
  addmeta = p: meta: p.overrideAttrs (old: {
    meta = (old.meta or { }) // meta;
  });
in

{
  # misc
  lipsum = pkgs.callPackage ./misc/lipsum { inherit addmeta; };

  # system
  nosleep = pkgs.callPackage ./system/nosleep { inherit addmeta; };
  toggle = pkgs.callPackage ./system/toggle { inherit addmeta; };

  # nix
  add-rpath = pkgs.callPackage ./nix/add-rpath { inherit addmeta; };

  # cs2110
  autograde = pkgs.callPackage ./cs2110/autograde { inherit addmeta; };
  csrh = lib.recurseIntoAttrs (pkgs.callPackage ./cs2110/csrh { inherit addmeta; });
}

