{ pkgs, ... }:

{
  imports = [
    ./fonts.nix
    ./compton.nix
    ./redshift.nix
  ];

  environment.systemPackages = with pkgs; [
    xorg.xev
    xorg.xmodmap
    xorg.xdpyinfo
    xdotool
  ];

  services.xserver = {

    enable = true;

    # Enable touchpad
    libinput = {
      enable = true;
      naturalScrolling = true;
    };

    layout = "us";
    xkbOptions = "caps:escape";

    desktopManager = {
      default = "none";
      xterm.enable = false;
    };

    windowManager.i3 = {
      enable = true;
      package = pkgs.i3-gaps;
      configFile = ./i3/i3config;

      # Set the desktop background to the current cached lock screen
      # TODO: consider using `feh --no-fehbg --bg-fill --randomize ../../desktop-backgrounds/*.png`
      # (or find a way to randomize betterlockscreen backgrounds)
      extraSessionCommands = ''
        ${pkgs.betterlockscreen}/bin/betterlockscreen -w
      '';

      extraPackages = with pkgs; [
        # Menu/launcher
        # j4 is slightly snappier and can read .desktop files, but dmenu is
        # still used to invoke bash commands
        dmenu j4-dmenu-desktop
        # Status bar
        i3status
        # Lock screen
        i3lock betterlockscreen
        # Applets
        networkmanagerapplet
        blueman
        # Media controls (pactl is provided by pulseaudio)
        playerctl
      ];
    };

  };

  programs = {
    nm-applet.enable = true;
  };
}
