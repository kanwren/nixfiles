{ pkgs, ... }:

{
  programs.rofi = {
    enable = true;
    terminal = "${pkgs.kitty}/bin/kitty";
    theme = ./catppuccin.rasi;
    font = "FiraCode Nerd Font 11";
    extraConfig = {
      show-icons = true;
      icon-theme = "hicolor";
      drun-display-format = "{name}";
      disable-history = false;
      fullscreen = false;
      hide-scrollbar = true;
      sidebar-mode = false;

      display-ssh = " [ssh]";
      display-run = " [run]";
      display-drun = "異 [drun]";
      display-window = " [window]";
      display-combi = " [combi]";
    };
  };
}

