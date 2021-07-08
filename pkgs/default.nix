{ pkgs ? import <nixpkgs> { }
, nur
, naersk
, fenix
}:

let
  inherit (pkgs) lib;
in
{
  # Re-export NUR as a package set
  nur = import nur { nurpkgs = pkgs; inherit pkgs; };

  # development
  spim = pkgs.callPackage ./development/spim { };

  # misc
  kakounePlugins = lib.recurseIntoAttrs {
    kak-mirror = pkgs.callPackage ./misc/kak-mirror { };
    kak-readline = pkgs.callPackage ./misc/kak-readline { };
    kak-smartindent = pkgs.callPackage ./misc/kak-smartindent { };
    kak-split-object = pkgs.callPackage ./misc/kak-split-object { };
    kakoune-themes = pkgs.callPackage ./misc/kakoune-themes { };
  };

  zshPlugins = lib.recurseIntoAttrs {
    zsh-vi-mode = pkgs.callPackage ./misc/zsh-vi-mode { };
  };

  tmuxPlugins = lib.recurseIntoAttrs {
    nord-tmux = pkgs.callPackage ./misc/nord-tmux {
      inherit (pkgs.tmuxPlugins) mkTmuxPlugin;
    };
  };

  nord-dircolors = pkgs.callPackage ./misc/nord-dircolors { };

  # tools
  globus-connect = pkgs.callPackage ./tools/globus-connect { };

  carbon-now-cli = pkgs.callPackage ./tools/carbon-now-cli {
    nodejs = pkgs.nodejs-14_x;
  };

  rust-script = pkgs.callPackage ./tools/rust-script {
    inherit naersk fenix;
  };

  # scripts
  scripts = lib.recurseIntoAttrs (pkgs.callPackage ./scripts {
    inherit naersk fenix;
  });
}

