# nixfiles

[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)

My NixOS configurations and other Nix files :snowflake:

### Overview

- [`flake.nix`](flake.nix): Flakes for each configuration, outputs for libs/modules/packages, and a dev shell for working with secrets. Some modules, overlays, and settings are applied in here.
- [`modules/`](modules/): Custom NixOS modules
- [`hm-modules/`](hm-modules/): Custom home-manager modules
- [`pkgs/`](pkgs/): Custom-built derivations exported from flake. Usually seen imported as `custom.pkgs` when used in configs.
- [`lib/`](lib/): Custom library functions used throughout the configs and exported from flake. Usually seen imported as `custom.lib` or `nlib`.
- [`installer/`](installer/): Minimal custom installer configuration using [nixos-generators](https://github.com/nix-community/nixos-generators); see the [`installer`](#installer) section
- `overlays/`: Nixpkgs overlays for overriding or adding packages
- [`secrets/`](secrets/): Secrets are managed using [sops-nix](https://github.com/Mic92/sops-nix)
- `templates/`: Nix flake templates, as used by `nix flake new`

### Hosts

- [`hecate`](hecate/): My main laptop
- [`homepi`](homepi/): My Raspberry Pi running [Home Assistant](https://www.home-assistant.io/)

#### hecate

The bulk of the configuration is for my main laptop `hecate`:

- [`configuration.nix`](configuration.nix): Configuration root; imports all other configuration.
- [`boot/`](boot/): Bootloader and emulation settings
- [`hardware/`](hardware/): General hardware-related configurations
  - Note that the bulk of the hardware configurations specific to `hecate` are in `flake.nix`, including [nixos-hardware](https://github.com/NixOS/nixos-hardware/) modules and `hardware-configuration.nix`.
- [`home/`](home/): Configurations for [home-manager](https://github.com/nix-community/home-manager/)
  - [`home/default.nix`](home/default.nix) is the configuration root
  - Each subdirectory is for configuring a different program/service
- [`i18n/`](i18n/): Language, input, and internationalization options
- [`networking/`](networking/): Wireless and firewall settings
- [`nix/`](nix/): Nix- and nixpkgs-related settings
  - [`default.nix`](default.nix): Main settings for nix and nixpkgs
  - [`caches.nix`](caches.nix): Cachix binary caches
  - [`overlays/`](overlays/): nixpkgs overlays for manually fixing and updating packages before they're fixed upstream
- [`pkgs/`](pkgs/): System-wide packages and some configurations for them
- [`security/`](security/): Miscellaneous security-related settings
- [`services/`](services/): Miscellaneous system programs and services
- [`time/`](time/): Settings related to system time
- [`users/`](users/): Set up all the users and groups
- [`virtualisation/`](virtualisation/): Configurations for programs used for containerization and virtualization (docker, podman, etc.)
- [`xserver/`](xserver/): Graphical configurations (display manager, window manager, fonts, applets, etc.)

### `installer`

`installer/` contains a custom installer configuration. This can be built
manually via [nixos-generators](https://github.com/nix-community/nixos-generators).
For example, if building on an `x86_64-linux` system:

```
# x86_64-linux installer iso
$ nix run 'github:nix-community/nixos-generators#nixos-generate' -- \
    -f install-iso -c installer/configuration.nix

# aarch64-linux installer sd image (requires 'boot.binfmt.emulatedSystems = [ "aarch64-linux" ];')
$ nix run 'github:nix-community/nixos-generators#nixos-generate' -- \
    -f sd-aarch64-installer --system aarch64-linux -c installer/configuration.nix
```

Alternatively, `legacyPackages` exports derivations to do this automatically:

```
$ nix build 'github:nprindle/nixfiles#legacyPackages.x86_64-linux.installer.install-iso'
$ nix build 'github:nprindle/nixfiles#legacyPackages.aarch64-linux.installer.sd-aarch64-installer'
```

### sops-nix

When editing sops files, keys in `secrets/keys/users` should automatically be
picked up when using the dev shell. To use a specific key for a file, set
`SOPS_PGP_FP` to the key's fingerprint.

To generate a PGP key for a new machine named `$HOSTNAME`:

```
# Drop into shell with sops-nix tools
$ sudo nix develop

# Generate the key
$ sops-init-gpg-key --hostname $HOSTNAME --gpghome /var/lib/sops

# Put generated key in secrets/keys/users/$HOSTNAME.asc
```

To import an existing armored private key `$HOSTNAME.asc`:

```
$ sudo nix develop
$ GNUPGHOME=/var/lib/sops gpg --import $HOSTNAME.asc
```

