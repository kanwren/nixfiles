{
  pkgs,
  config,
  lib,
  ...
}: let
  cfg = config.mixins.gpg-agent;
in {
  options.mixins.gpg-agent.enable = lib.mkOption {
    type = lib.types.bool;
    default = config.mixins.enable;
    description = "Whether to enable the gpg-agent mixin";
  };

  config = lib.mkIf cfg.enable {
    services.gpg-agent = {
      enable = true;
      enableSshSupport = true;
      pinentry.package = pkgs.pinentry;

      defaultCacheTtl = 7 * 24 * 60 * 60; # one week
      defaultCacheTtlSsh = config.services.gpg-agent.defaultCacheTtl;
      maxCacheTtl = config.services.gpg-agent.defaultCacheTtl;
      maxCacheTtlSsh = config.services.gpg-agent.defaultCacheTtl;
    };
  };
}
