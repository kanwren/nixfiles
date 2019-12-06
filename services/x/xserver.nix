{ config, pkgs, ... }:

{
  services.xserver = {

    enable = true;

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
      configFile = ./i3/config;
      extraSessionCommands =
        let bg = ./i3/desktop_bg.png;
        in pkgs.lib.optionalString (builtins.pathExists bg) ''
          ${pkgs.feh}/bin/feh --bg-fill ${builtins.toString bg}
        '';
      extraPackages = with pkgs; [
        dmenu
        i3status
        i3lock
        networkmanagerapplet
      ];
    };

  };
}
