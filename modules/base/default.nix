{
  inputs,
  config,
  ...
}@toplevel:

let
  rev = inputs.self.rev or inputs.self.dirtyRev or null;
in
{
  flake.modules = {
    nixos.base =
      { pkgs, lib, ... }:
      {
        imports = [
          inputs.disko.nixosModules.disko
          config.flake.modules.nixos.gpg
        ];

        system = {
          stateVersion = lib.mkDefault "25.05";
          configurationRevision = rev;
          autoUpgrade.enable = false;
        };

        nixpkgs.overlays = [
          config.flake.overlays.default
          config.flake.overlays.pkgs-unstable
        ];

        boot = {
          kernelPackages = pkgs.linuxPackages_latest;
          tmp.cleanOnBoot = true;
        };

        console = {
          font = "Lat2-Terminus16";
          keyMap = "us";
        };

        documentation.man.cache.enable = false;

        i18n.defaultLocale = "en_US.UTF-8";

        networking = {
          firewall.enable = true;
          networkmanager.enable = true;
        };

        programs.command-not-found.enable = false;

        security = {
          sudo = {
            enable = true;
            extraConfig = ''
              Defaults lecture = never
            '';
            wheelNeedsPassword = true;
          };

          rtkit.enable = true;
        };

        services = {
          atd.enable = true;
          gnome.gnome-keyring.enable = true;
          udev.enable = true;
        };

        virtualisation = {
          podman = {
            enable = true;
            autoPrune = {
              enable = true;
              dates = "weekly";
            };
          };
        };

        users.mutableUsers = true; # TODO: disable
      };

    darwin.base =
      { config, ... }:
      {
        imports = [
          toplevel.config.flake.modules.darwin.gpg
        ];

        system = {
          stateVersion = 5;
          configurationRevision = rev;
        };

        networking = {
          localHostName = config.networking.computerName;
          hostName = "${config.nentworking.localHostName}.local";
        };

        nixpkgs.overlays = [
          config.flake.overlays.default
          config.flake.overlays.pkgs-unstable
        ];

        environment.variables = {
          LC_CTYPE = "en_US.UTF-8";
        };
      };
  };
}
