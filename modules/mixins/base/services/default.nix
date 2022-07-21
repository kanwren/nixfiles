{ config, ... }:

{
  services = {
    openssh = {
      enable = true;
      permitRootLogin = "prohibit-password";
      passwordAuthentication = false;
    };
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

