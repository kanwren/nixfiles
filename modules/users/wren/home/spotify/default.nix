# See https://github.com/spicetify/spicetify-cli/issues/1453

{ pkgs, self, system, ... }:

let
  catppuccin = self.packages.${system}.catppuccin-spicetify;
in

{
  programs.spicetify = {
    enable = true;

    currentTheme = "catppuccin";
    extensions = [ "catppuccin.js" ];

    addons = [ catppuccin ];
  };
}

