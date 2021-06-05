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
  serve = pkgs.callPackage ./util/serve { inherit haskellScript addmeta; };
  truthtable = pkgs.callPackage ./util/truthtable { inherit haskellScript addmeta; };
  lipsum = pkgs.callPackage ./util/lipsum { inherit addmeta; };

  # system
  nosleep = pkgs.callPackage ./system/nosleep { inherit addmeta; };
  toggle = pkgs.callPackage ./system/toggle { inherit addmeta; };

  # nix
  nix-gcroots = pkgs.callPackage ./nix/nix-gcroots { inherit addmeta; };
  comma = pkgs.callPackage ./nix/comma { inherit addmeta; };
  add-rpath = pkgs.callPackage ./nix/add-rpath { inherit addmeta; };

  # cs2110
  autograde = pkgs.callPackage ./cs2110/autograde { inherit addmeta; };
  csrh = lib.recurseIntoAttrs (pkgs.callPackage ./cs2110/csrh { inherit addmeta; });

  # haskell
  ghcWithPackages = pkgs.callPackage ./haskell/ghcWithPackages { inherit addmeta; };
  hoogleServer = pkgs.callPackage ./haskell/hoogleServer { inherit addmeta; };
}

