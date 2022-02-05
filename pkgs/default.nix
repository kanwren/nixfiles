{ pkgs ? import <nixpkgs> { }
, naersk
, fenix
}:

let
  inherit (pkgs) lib;
in
{
  # development
  spim = pkgs.callPackage ./development/spim { };

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
  scripts = lib.recurseIntoAttrs (pkgs.callPackage ./scripts { });
}

