{ pkgs, config, ... }:

{
  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
    pinentryPackage = pkgs.pinentry;

    defaultCacheTtl = 7 * 24 * 60 * 60; # one week
    defaultCacheTtlSsh = config.services.gpg-agent.defaultCacheTtl;
    maxCacheTtl = config.services.gpg-agent.defaultCacheTtl;
    maxCacheTtlSsh = config.services.gpg-agent.defaultCacheTtl;
  };
}

