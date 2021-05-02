{ pkgs ? import <nixpkgs> {}
}:

{
  kakounePlugins = {
    kak-mirror = pkgs.callPackage ./kak-mirror {};
    kak-readline = pkgs.callPackage ./kak-readline {};
    kak-smartindent = pkgs.callPackage ./kak-smartindent {};
    kakoune-themes = pkgs.callPackage ./kakoune-themes {};
  };

  zshPlugins = {
    zsh-vi-mode = pkgs.callPackage ./zsh-vi-mode {};
  };
}
