{ config, pkgs, ... }:

{
  services = {
    # List services that you want to enable:

    # Enable the OpenSSH daemon.
    openssh.enable = true;

    # Enable CUPS to print documents.
    printing.enable = true;

    xserver = {
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
        extraPackages = with pkgs; [
          dmenu
          i3status
          i3lock
          networkmanagerapplet
        ];
      };
    };

    compton = {
      enable = true;

      # activeOpacity = "0.85";
      # inactiveOpacity = "0.9";
      opacityRules = [
        "85:class_g = 'Alacritty'"
      ];

      shadow = true;

      # 4ms fade when changing opacity
      fade = true;
      fadeDelta = 4;
    };
  };
}
