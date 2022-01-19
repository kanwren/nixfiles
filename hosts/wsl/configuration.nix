{ lib, ... }:

{
  boot.wsl = {
    enable = true;
    user = "nprin";
  };

  # this wouldn't work, so override the base module
  security.sudo.wheelNeedsPassword = lib.mkForce false;
  environment.noXlibs = false; # TODO: remove gtk dependencies

  users = {
    mutableUsers = true;
    users.nprin = {
      initialPassword = "setup";
    };
  };
}
