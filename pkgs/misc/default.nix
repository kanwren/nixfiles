{ pkgs ? import <nixpkgs> {}
}:

let
  inherit (pkgs) lib;
in
{
  kakounePlugins = lib.recurseIntoAttrs {
    kak-mirror = pkgs.callPackage ./kak-mirror {};
    kak-readline = pkgs.callPackage ./kak-readline {};
    kak-smartindent = pkgs.callPackage ./kak-smartindent {};
    kakoune-themes = pkgs.callPackage ./kakoune-themes {};
  };

  zshPlugins = lib.recurseIntoAttrs {
    zsh-vi-mode = pkgs.callPackage ./zsh-vi-mode {};
  };
}
