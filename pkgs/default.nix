{ pkgs ? import <nixpkgs> { }
, naersk
, fenix
}:

let
  inherit (pkgs) lib;
  addmeta = p: meta: p.overrideAttrs (old: {
    meta = (old.meta or { }) // meta;
  });
in
{
  # development
  spim = pkgs.callPackage ./development/spim { };

  zshPlugins = lib.recurseIntoAttrs {
    zsh-vi-mode = pkgs.callPackage ./misc/zsh-vi-mode { };
  };

  nord-dircolors = pkgs.callPackage ./misc/nord-dircolors { };

  catppuccin-kitty = pkgs.callPackage ./misc/catppuccin-kitty { };
  catppuccin-tmux = pkgs.callPackage ./misc/catppuccin-tmux { };
  catppuccin-zathura = pkgs.callPackage ./misc/catppuccin-zathura { };

  # tools
  globus-connect = pkgs.callPackage ./tools/globus-connect { };

  rust-script = pkgs.callPackage ./tools/rust-script {
    inherit naersk fenix;
  };

  autograde = pkgs.callPackage ./tools/cs2110/autograde { inherit addmeta; };
  csrh = pkgs.callPackage ./tools/cs2110/csrh { inherit addmeta; };

  # scripts
  scripts = lib.recurseIntoAttrs {
    lipsum = pkgs.callPackage ./scripts/lipsum { inherit addmeta; };
    nosleep = pkgs.callPackage ./scripts/nosleep { inherit addmeta; };
    add-rpath = pkgs.callPackage ./scripts/add-rpath { inherit addmeta; };
  };
}

