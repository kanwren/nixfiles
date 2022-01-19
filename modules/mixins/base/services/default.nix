{ pkgs, lib, ... }:

{
  services = {
    # Enable the OpenSSH daemon
    openssh = {
      enable = true;
      forwardX11 = true;
      allowSFTP = true;
    };
  };

  programs = {
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
      pinentryFlavor = "gtk2";
    };
  };
}

