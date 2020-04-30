{ config, lib, ... }:

let
  cfg = config.services.onedrive;

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
    enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Enable OneDrive service";
    };
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ pkgs.onedrive ];

    systemd.user.services.onedrive-sync = {
      wantedBy = [ "graphical-session.target" ];
      serviceConfig = {
        Type = "simple";
        ExecStart = ''
          ${pkgs.onedrive}/bin/onedrive --monitor --verbose --confdir=%h/.config/%i
        '';
        Restart = "on-failure";
        RestartSec = 3;
        RestartPreventExitStatus = 3;
      };
      description = "OneDrive Free Client";
      documentation = [ "https://github.com/abraunegg/onedrive" ];
    };
  };
}
