_:

{
  flake.modules = {
    nixos.gpg =
      { pkgs, ... }:
      {
        environment.systemPackages = [ pkgs.gnupg ];

        programs.gnupg.agent = {
          enable = true;
          enableSSHSupport = true;
          pinentryPackage = pkgs.pinentry-curses;
        };
      };

    darwin.gpg =
      { pkgs, ... }:
      {
        environment.systemPackages = [
          pkgs.gnupg
          pkgs.pinentry_mac
        ];

        programs.gnupg.agent.enable = true;
      };

    homeManager.gpg =
      { pkgs, config, ... }:
      {
        services.gpg-agent = {
          enable = true;
          enableSshSupport = true;
          pinentry.package = pkgs.pinentry-curses;
          defaultCacheTtl = 7 * 24 * 60 * 60;
          defaultCacheTtlSsh = config.services.gpg-agent.defaultCacheTtl;
          maxCacheTtl = config.services.gpg-agent.defaultCacheTtl;
          maxCacheTtlSsh = config.services.gpg-agent.maxCacheTtl;
        };
      };
  };
}
