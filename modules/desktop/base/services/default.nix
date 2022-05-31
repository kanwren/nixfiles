{ pkgs, lib, ... }:

{
  services = {
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

    atd.enable = true;
  };

  # open port 631 for cups
  networking.firewall = {
    allowedTCPPorts = [ 631 ];
    allowedUDPPorts = [ 631 ];
  };

  programs = {
    nm-applet.enable = true;

    wireshark = {
      enable = true;
      package = pkgs.wireshark-qt;
    };
  };
}

