# Enabling and configuring various system programs

{ config, pkgs, ... }:

# Some programs need SUID wrappers, can be configured further or are started in
# user sessions.
{
  programs = {

    mtr.enable = true;

    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
    };

  };
}

