# Build with, e.g.:
#   nix-build aarch64-iso.nix
# This will fill in nixpkgs by default, e.g.:
#   nix-build minimal-iso.nix --arg nixpkgs "<nixpkgs>"
{ nixpkgs ? <nixpkgs>
, system  ? "aarch64-linux"
}:

let
  minimal-nixos-iso-config = import "${nixpkgs}/nixos" {

    inherit system;

    configuration = { config, pkgs, lib, ... }: {
      imports = [
        "${nixpkgs}/nixos/modules/installer/cd-dvd/sd-image-aarch64.nix"
        # Provide a copy of the NixOS channel
        "${nixpkgs}/nixos/modules/installer/cd-dvd/channel.nix"
      ];
      environment.systemPackages = with pkgs; [
        wget curl manpages git vim
        mkpasswd
      ];
    };

  };
in
{
  iso = minimal-nixos-iso-config.config.system.build.sdImage;
}

