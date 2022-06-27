{ pkgs ? import <nixpkgs> { } }:

let
  inherit (pkgs) lib;
  addmeta = p: meta: p.overrideAttrs (old: {
    meta = (old.meta or { }) // meta;
  });
in
{
  catppuccin-kitty = pkgs.callPackage ./misc/catppuccin-kitty { };
  catppuccin-tmux = pkgs.callPackage ./misc/catppuccin-tmux { };
  catppuccin-zathura = pkgs.callPackage ./misc/catppuccin-zathura { };
  catppuccin-cava = pkgs.callPackage ./misc/catppuccin-cava { };
  catppuccin-btop = pkgs.callPackage ./misc/catppuccin-btop { };

  # tools
  globus-connect = pkgs.callPackage ./tools/globus-connect { };

  # scripts
  scripts = lib.recurseIntoAttrs {
    lipsum = pkgs.callPackage ./scripts/lipsum { inherit addmeta; };
    nosleep = pkgs.callPackage ./scripts/nosleep { inherit addmeta; };
    add-rpath = pkgs.callPackage ./scripts/add-rpath { inherit addmeta; };
  };
}

