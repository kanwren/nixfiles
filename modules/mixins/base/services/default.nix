{ config, ... }:

{
  services = {
    openssh = {
      enable = true;
      settings = {
        PermitRootLogin = "prohibit-password";
        PasswordAuthentication = false;
      };
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

