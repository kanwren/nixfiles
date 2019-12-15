# Edit this configuration file to define what should be installed on
# your system. Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports = [
    ./hardware-configuration.nix
    ./boot/default.nix
    ./nix/default.nix
    ./hardware/default.nix
    ./time/default.nix
    ./i18n/default.nix
    ./services/default.nix
    ./users/default.nix
    ./pkgs/default.nix
    ./networking/default.nix
    ./home/default.nix
  ];

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.09"; # Did you read the comment?

}

