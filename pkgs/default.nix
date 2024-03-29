{ pkgs ? import <nixpkgs> { }
, sources
}:

let
  inherit (pkgs) lib;
  addmeta = p: meta: p.overrideAttrs (old: {
    meta = (old.meta or { }) // meta;
  });
in
rec {
  catppuccin-kitty = pkgs.callPackage ./misc/catppuccin-kitty { inherit (sources) catppuccin-kitty-src; };
  catppuccin-tmux = pkgs.callPackage ./misc/catppuccin-tmux { inherit (sources) catppuccin-tmux-src; };
  catppuccin-zathura = pkgs.callPackage ./misc/catppuccin-zathura { inherit (sources) catppuccin-zathura-src; };
  catppuccin-cava = pkgs.callPackage ./misc/catppuccin-cava { inherit (sources) catppuccin-cava-src; };
  catppuccin-btop = pkgs.callPackage ./misc/catppuccin-btop { inherit (sources) catppuccin-btop-src; };
  catppuccin-twemoji-hearts = pkgs.callPackage ./misc/catppuccin-twemoji-hearts {
    inherit generate-heart-emoji;
  };

  wd-fish = pkgs.fishPlugins.callPackage ./misc/wd-fish { inherit (sources) wd-fish-src; };

  # scripts
  lipsum = pkgs.callPackage ./scripts/lipsum { inherit addmeta; };
  generate-heart-emoji = pkgs.callPackage ./scripts/generate-heart-emoji { inherit addmeta; };
  jj-helpers = pkgs.callPackage ./scripts/jj-helpers { };

  # tools
  slides-full = pkgs.callPackage ./tools/slides { };
  envtpl = pkgs.callPackage ./tools/envtpl { inherit (sources) envtpl-src; };
  frum = pkgs.callPackage ./tools/frum { inherit (sources) fenix naersk frum-src; };
}

