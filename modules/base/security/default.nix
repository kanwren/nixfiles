{ pkgs, lib, ... }:

{
  security = {
    sudo = {
      enable = true;
      package = pkgs.sudo;
      wheelNeedsPassword = true;
    };
    chromiumSuidSandbox.enable = false; # TODO
  };
}
