{ ... }:

let
  # ldc/dmd break a lot, so pin nixpkgs for reproducibility
  fetchNixpkgs = { rev, sha256 }: builtins.fetchTarball {
    url = "https://github.com/nixos/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256;
  };
  pkgs = import (fetchNixpkgs {
    rev = "c039152ba8619ecbddf743d182200f19a2514476";
    sha256 = "0z3md7ccirs3855339x3immxw5ahs8wb4vp1is2xylwq8qkkss0s";
  }) {};
in {
  environment.systemPackages = [ pkgs.onedrive ];

  systemd.user.services.onedrive-sync = {
    script = "${pkgs.onedrive}/bin/onedrive --monitor";
    serviceConfig = {
      Restart = "no";
    };
    description = "OneDrive Free Client";
    documentation = [ "https://github.com/abraunegg/onedrive" ];
  };
}
