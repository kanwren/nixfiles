{ pkgs, lib, ... }:

{
  security = {
    sudo = {
      enable = true;
      package = pkgs.sudo.override { withInsults = true; };
      wheelNeedsPassword = true;
      # Enable insults (requires sudo compiled with insults)
      extraConfig = lib.mkAfter ''
        Defaults insults
      '';
    };
  };
}
