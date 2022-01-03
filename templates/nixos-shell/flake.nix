# Initialize a config with
# $ nix flake init -t 'github:nprindle/nixfiles#nixos-shell'
# Edit the config, then run with
# $ nix run 'github:Mic92/nixos-shell' -- --flake '.#vm'

{
  description = "Template for nixos-shell VMs";

  inputs = {
    nixos-shell.url = "github:Mic92/nixos-shell";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, nixos-shell, flake-utils, flake-compat }:
    let
      inherit (nixpkgs) lib;
    in
    {
      nixosConfigurations.vm = lib.makeOverridable lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ({ pkgs, config, lib, ... }: {
            environment.systemPackages = with pkgs; [ vim ];

            nixos-shell.mounts = {
              mountHome = false;
              mountNixProfile = false;
              extraMounts = {
                # "/media" = /media;
                # "/var/www" = { target = ./src; cache = "none"; };
              };
            };

            virtualisation = {
              memorySize = 4 * 1024; # MB
              diskSize = 10 * 1024; # MB
              cores = 1;
              qemu.networkingOptions = [
                # "hostfwd=tcp::2222-:22" # forward port 22 to port 2222
              ];
            };

            # graphics
            # virtualisation.graphics = true;
            # services.xserver.enable = true;
            # services.xserver.windowManager.i3.enable = true;

            networking.firewall.enable = true;
            nix = {
              extraOptions = ''
                experimental-features = nix-command flakes ca-derivations
              '';
              registry = { nixpkgs.flake = nixpkgs; };
              nixPath = [ "nixpkgs=${pkgs.path}" ];
            };
            services.openssh.enable = true;
          })

          nixos-shell.nixosModules.nixos-shell
        ];
      };
    };
}
