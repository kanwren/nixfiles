{ pkgs, lib, ... }:

{
  services = {
    # Enable lorri daemon for nix/direnv integration
    lorri.enable = true;

    # Enable the OpenSSH daemon
    openssh.enable = true;

    # Enable CUPS to print documents
    printing = {
      enable = true;
    };

    avahi = {
      enable = true;
      publish = {
        enable = true;
        userServices = true;
      };
      nssmdns = true;
    };

    jack = {
      # jackd.enable = true;
    };

    mysql = {
      enable = true;
      package = pkgs.mysql;
    };

    # Bluetooth manager (or use bluetoothctl, but this has a nice applet)
    blueman.enable = true;

    gnome3 = {
      gnome-keyring.enable = true;
    };

    onedrive.enable = true;
  };

  # Some programs need SUID wrappers, can be configured further or are started
  # in user sessions.
  programs = {
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
      pinentryFlavor = "curses";
    };

    nm-applet.enable = true;
    mtr.enable = true;
  };
}
