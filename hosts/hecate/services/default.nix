{ pkgs, config, ... }:

{
  services = {
    openssh = {
      enable = true;
      allowSFTP = true;
      settings = {
        PermitRootLogin = "prohibit-password";
        PasswordAuthentication = false;
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

    pueue.enable = true;
  };

  programs = {
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
      pinentryPackage = pkgs.pinentry;
    };

    nm-applet.enable = true;

    wireshark = {
      enable = true;
      package = pkgs.wireshark-qt;
    };
  };

  # open port 631 for cups
  networking.firewall = {
    allowedTCPPorts = [ 631 ];
    allowedUDPPorts = [ 631 ];
  };
}

