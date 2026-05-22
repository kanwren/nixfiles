{
  flake.modules = {
    nixos.base = {
      programs = {
        fuse.userAllowOther = true;
      };

      services.openssh = {
        enable = true;
        allowSFTP = true;
        settings = {
          PermitRootLogin = "prohibit-password";
          PasswordAuthentication = false;
          X11Forwarding = true;
        };
      };
    };
  };
}
