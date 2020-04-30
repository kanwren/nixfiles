{ ... }:

{
  imports = [
    ./onedrive.nix
  ];

  services = {
    # Enable the OpenSSH daemon
    openssh.enable = true;

    # Enable CUPS to print documents
    printing.enable = true;

    # Bluetooth manager (or use bluetoothctl, but this has a nice applet)
    blueman.enable = true;

    # Show the NixOS manual on virtual console 8
    nixosManual = {
      showManual = true;
      ttyNumber = 8;
    };

    # Provided by ./onedrive.nix
    onedrive = {
      enable = true;
    };
  };

  # Some programs need SUID wrappers, can be configured further or are started
  # in user sessions.
  programs = {
    nm-applet.enable = true;
    mtr.enable = true;

    ssh = {
      agentTimeout = null;
      startAgent = true;
    };
  };
}
