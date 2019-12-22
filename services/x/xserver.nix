{ config, pkgs, lib, ... }:

{
  services.xserver = {

    enable = true;

    # Enable touchpad
    libinput.enable = true;

    layout = "us";
    xkbOptions = "caps:escape";

    desktopManager = {
      default = "none";
      xterm.enable = false;
    };

    windowManager.i3 = {
      enable = true;
      package = pkgs.i3-gaps;
      configFile = ./i3config;

      # Make sure the desktop background is loaded, if it exists
      extraSessionCommands =
      let
        backgrounds = ../../desktop-backgrounds;
        backgroundsPath = builtins.toString backgrounds;
      in lib.optionalString (builtins.pathExists backgrounds) ''
        ${pkgs.feh}/bin/feh --no-fehbg --bg-fill --randomize ${backgroundsPath}/*.png
      '';

      extraPackages = with pkgs; [
        # j4 is slightly snappier and can read .desktop files, but dmenu is
        # still used to invoke bash commands
        dmenu
        j4-dmenu-desktop
        i3status
        i3lock
        # Applets
        networkmanagerapplet
        blueman
      ];
    };

  };

  programs = {
    nm-applet.enable = true;
  };
}
