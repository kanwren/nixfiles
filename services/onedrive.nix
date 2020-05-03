{ config, lib, ... }:

let
  cfg = config.services.onedrive;

  # Replicate a string 'n' times with spaces in between
  replicateStr = n: str: builtins.concatStringsSep " " (builtins.genList (lib.const str) n);

  # ldc/dmd break a lot, so temporarily pin nixpkgs for reproducibility
  fetchNixpkgs = { rev, sha256 }: builtins.fetchTarball {
    url = "https://github.com/nixos/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  };
  pkgs = import (fetchNixpkgs {
    rev = "c039152ba8619ecbddf743d182200f19a2514476";
    sha256 = "0z3md7ccirs3855339x3immxw5ahs8wb4vp1is2xylwq8qkkss0s";
  }) {};

in {
  options.services.onedrive = {
    enable = lib.mkEnableOption "Enable OneDrive service";

    monitorInterval = lib.mkOption {
      type = lib.types.nullOr lib.types.int;
      default = null;
      description = "Number of seconds by which each sync operation is undertaken when idle under monitor mode";
    };

    verbosity = lib.mkOption {
      type = lib.types.ints.between 0 2;
      default = 1;
      description = "The amount of detail to log";
    };
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ pkgs.onedrive ];

    systemd.user.services.onedrive-sync = {
      description = "OneDrive Free Client";
      documentation = [ "https://github.com/abraunegg/onedrive" ];

      wantedBy = [ "default.target" ];
      wants = [ "network-online.target" ];
      after = [ "network-online.target" ];

      serviceConfig = {
        ExecStart = ''
          ${pkgs.onedrive}/bin/onedrive --monitor \
          ${replicateStr cfg.verbosity " --verbose"} \
          --confdir=%h/.config/onedrive \
          ${lib.optionalString (cfg.monitorInterval != null) "--monitor-interval ${toString cfg.monitorInterval}"}
        '';
        Restart = "on-failure";
        RestartSec = 3;
        RestartPreventExitStatus = 3;
      };
    };
  };
}
