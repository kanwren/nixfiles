# Build with, e.g.:
#   nix-build minimal-iso.nix
# This will fill in nixpkgs and the current system by default, e.g.:
#   nix-build minimal-iso.nix --arg nixpkgs "<nixpkgs>" --argstr system x86_64-linux
{ nixpkgs ? <nixpkgs>
, system ? builtins.currentSystem
}:

let
  minimal-nixos-iso-config = import "${nixpkgs}/nixos" {

    inherit system;

    configuration = { config, pkgs, ... }: {
      imports = [
        "${nixpkgs}/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix"
      ];
      environment.systemPackages = with pkgs; [
        wget curl manpages git vim
      ];
      networking = {
        networkmanager.enable = true;
        wireless.enable = false;
      };
    };

  };
in
{
  iso = minimal-nixos-iso-config.config.system.build.isoImage;
}
