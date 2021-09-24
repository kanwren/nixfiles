{ pkgs, lib, ... }:

{
  imports = [
    ./tailscale.nix
  ];

  services = {
    # Enable the OpenSSH daemon
    openssh = {
      enable = true;
      forwardX11 = true;
      allowSFTP = true;
    };

    # Bluetooth manager (or use bluetoothctl, but this has a nice applet)
    blueman.enable = true;

    # cups
    printing = {
      enable = true;
    };

    gnome = {
      gnome-keyring.enable = true;
    };

    postgresql = {
      enable = true;
    };
  };

  programs = {
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
      pinentryFlavor = "gtk2";
    };

    nm-applet.enable = true;

    wireshark = {
      enable = true;
      package = pkgs.wireshark-qt;
    };
  };
}
