{ pkgs, ... }:

{
  home.packages = with pkgs; [
    discord
    betterdiscordctl
  ];

  # it's weird for this to be a directory, but betterdiscord can't read theme
  # files that are symlinks, but does support the themes directory itself being
  # a symlink
  xdg.configFile."BetterDiscord/themes".source = ./themes;
}

