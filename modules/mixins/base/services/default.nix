{ pkgs, lib, config, ... }:

{
  services = {
    openssh.enable = true;
  };

  programs = {
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
      pinentryFlavor =
        if config.services.xserver.enable then
          "gtk2"
        else
          "curses";
    };
  };
}

