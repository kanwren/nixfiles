{ pkgs, lib, ... }:

{
  services = {
    # Enable the OpenSSH daemon
    openssh.enable = true;

    # Enable CUPS to print documents
    printing = {
      enable = true;
    };

    # Bluetooth manager (or use bluetoothctl, but this has a nice applet)
    blueman.enable = true;

    gnome3 = {
      gnome-keyring.enable = true;
    };

    onedrive.enable = true;
  };

  programs = {
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
      pinentryFlavor = "curses";
    };
    nm-applet.enable = true;
  };
}
