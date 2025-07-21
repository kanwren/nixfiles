{ pkgs, ... }:

{
  imports = [
    ./llm.nix
  ];

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
    printing = {
      enable = true;
      openFirewall = true;
    };

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
  };
}

