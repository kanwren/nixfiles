{ lib, ... }:

{
  system.stateVersion = "22.11";

  wsl = {
    enable = true;
    defaultUser = "wren";
    automountPath = "/mnt";
  };

  # this wouldn't work, so override the base module
  security.sudo.wheelNeedsPassword = lib.mkForce false;
  environment.noXlibs = false; # TODO: remove gtk dependencies

  users = {
    mutableUsers = true;
    users.wren = {
      initialPassword = "setup";
    };
  };
}
