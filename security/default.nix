{ config, pkgs, lib, ... }:

{
  security = {

    sudo = {
      enable = true;
      wheelNeedsPassword = true;
      # Enable insults (requires sudo compiled with insults)
      extraConfig = lib.mkAfter ''
        Defaults insults
      '';
    };

    chromiumSuidSandbox.enable = true;

  };
}
