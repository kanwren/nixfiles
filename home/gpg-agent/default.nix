{ config, pkgs, ... }:

{
  home-manager.users.nprin = {
    services.gpg-agent = {
      enable = true;
      enableSshSupport = true;
      verbose = true;
      # default cache time = 1 day
      defaultCacheTtl = 86400;
      defaultCacheTtlSsh = 86400;
      # max cache time = 30 days
      maxCacheTtl = 2592000;
      maxCacheTtlSsh = 2592000;
    };
  };
}


