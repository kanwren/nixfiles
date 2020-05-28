{ pkgs, ... }:

let
  # Need to do this instead of using overlay to avoid infinite recursion
  # See https://github.com/nix-community/nur#using-modules-overlays-or-library-functions-in-nixos
  nur = import (builtins.fetchTarball {
    url = "https://github.com/nix-community/NUR/archive/8ac25f6b5af24f829872c88fefbd6c434645a271.tar.gz";
    sha256 = "0wzjmx9vwsc7sb6has5vzj7nrl4cm91l01hflldr82g0v3s99nsg";
  }) { inherit pkgs; };
in {
  imports = [
    (nur.repos.nprindle.modules.onedrive)
  ];

  services = {
    # Enable lorri daemon for nix/direnv integration
    lorri.enable = true;

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
      monitorInterval = 60;
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
