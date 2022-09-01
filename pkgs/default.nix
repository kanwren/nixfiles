{ pkgs ? import <nixpkgs> { } }:

let
  inherit (pkgs) lib;
  addmeta = p: meta: p.overrideAttrs (old: {
    meta = (old.meta or { }) // meta;
  });
in
rec {
  catppuccin-kitty = pkgs.callPackage ./misc/catppuccin-kitty { };
  catppuccin-tmux = pkgs.callPackage ./misc/catppuccin-tmux { };
  catppuccin-zathura = pkgs.callPackage ./misc/catppuccin-zathura { };
  catppuccin-cava = pkgs.callPackage ./misc/catppuccin-cava { };
  catppuccin-btop = pkgs.callPackage ./misc/catppuccin-btop { };
  catppuccin-spicetify = pkgs.callPackage ./misc/catppuccin-spicetify { };
  catppuccin-twemoji-hearts = pkgs.callPackage ./misc/catppuccin-twemoji-hearts {
    inherit (scripts) generate-heart-emoji;
  };

  # scripts
  scripts = lib.recurseIntoAttrs {
    lipsum = pkgs.callPackage ./scripts/lipsum { inherit addmeta; };
    add-rpath = pkgs.callPackage ./scripts/add-rpath { inherit addmeta; };
    generate-heart-emoji = pkgs.callPackage ./scripts/generate-heart-emoji { inherit addmeta; };
  };
}

