{ pkgs, ... }:

let
  # Need to do this instead of using overlay to avoid infinite recursion
  # See https://github.com/nix-community/nur#using-modules-overlays-or-library-functions-in-nixos
  nur = import ../common/nur.nix { inherit pkgs; };
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
    printing = {
      enable = true;
      browsing = true;
      defaultShared = true;
    };

    avahi = {
      enable = true;
      publish = {
        enable = true;
        userServices = true;
      };
      nssmdns = true;
    };

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
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
      pinentryFlavor = "curses";
    };

    nm-applet.enable = true;
    mtr.enable = true;
  };
}
