{ pkgs, ... }:

{
  services = {
    openssh = {
      enable = true;
      allowSFTP = true;
      settings = {
        X11Forwarding = true;
      };
    };

    # Bluetooth manager (or use bluetoothctl, but this has a nice applet)
    blueman.enable = true;

    # cups
    printing.enable = true;

    gnome.gnome-keyring.enable = true;

    atd.enable = true;

    keybase.enable = true;
    kbfs.enable = true;
  };

  environment.systemPackages = [ pkgs.keybase-gui ];

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

