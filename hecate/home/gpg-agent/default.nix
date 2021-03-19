{ nlib, ... }:

{ config, pkgs, ... }:

let
  inherit (nlib) time;
in

{
  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
    pinentryFlavor = "curses";

    defaultCacheTtl = time.unitsOf time.seconds (time.weeks 1);
    defaultCacheTtlSsh = config.services.gpg-agent.defaultCacheTtl;
    maxCacheTtl = config.services.gpg-agent.defaultCacheTtl;
    maxCacheTtlSsh = config.services.gpg-agent.defaultCacheTtl;
  };
}

