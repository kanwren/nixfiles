# See https://github.com/spicetify/spicetify-cli/issues/1453

{ self, pkgs, ... }:

let
  catppuccin = self.packages.${pkgs.system}.catppuccin-spicetify;
in

{
  programs.spicetify = {
    enable = true;

    currentTheme = "catppuccin";
    extensions = [ "catppuccin.js" ];

    addons = [ catppuccin ];
  };
}

