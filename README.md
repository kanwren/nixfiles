# nixos-configs

My NixOS configurations :snowflake:

### Overview

- `flake.nix`: Flakes for each configuration, outputs for libs and modules, and
    a dev shell for working with secrets. Some modules, overlays, and settings
    are applied in here.
- `hm-modules/`: Custom home-manager modules
- `lib/`: Custom library functions used throughout the configs. Usually seen imported as `nlib`.
- `installer/`: Custom minimal installer configuration.
    `installer/configuration.nix` contains instructions for building installer
    ISOs/SD images using
    [nixos-generators](https://github.com/nix-community/nixos-generators)
- `secrets/`: Secrets are managed using [sops-nix](https://github.com/Mic92/sops-nix)

### Hosts

- `hecate`: My main laptop
- `homepi`: My Raspberry Pi running [Home Assistant](https://www.home-assistant.io/)

#### hecate

The bulk of the configuration is for my main laptop `hecate`:

- `configuration.nix`: Configuration root; imports all other configuration.
- `boot/`: Bootloader and emulation settings
- `hardware/`: General hardware-related configurations
  - Note that the bulk of the hardware configurations specific to `hecate` are
    in `flake.nix`, including [nixos-hardware](https://github.com/NixOS/nixos-hardware/)
    modules and `hardware-configuration.nix`.
- `home/`: Configurations for [home-manager](https://github.com/nix-community/home-manager/)
  - `home/default.nix` is the configuration root
  - Each subdirectory is for configuring a different program/service
- `i18n/`: Language, input, and internationalization options
- `networking/`: Wireless and firewall settings
- `nix/`: Nix- and nixpkgs-related settings
  - `default.nix`: Main settings for nix and nixpkgs
  - `caches.nix`: Cachix binary caches
  - `overlays/`: nixpkgs overlays for manually fixing and updating packages
    before they're fixed upstream
- `pkgs/`: System-wide packages and some configurations for them
  - `scripts/`: Utility scripts in several languages, installed as system packages
- `security/`: Miscellaneous security-related settings
- `services/`: Miscellaneous system programs and services
- `time/`: Settings related to system time
- `users/`: Set up all the users and groups
- `virtualisation/`: Configurations for programs used for containerization and
  virtualization (docker, podman, etc.)
- `xserver/`: Graphical configurations (display manager, window manager, fonts, applets, etc.)

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

