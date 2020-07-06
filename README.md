# nixos-configs

My NixOS configurations. Configs are split up across multiple directories:

- `boot`: Configuration of the bootloader and boot process
- `hardware`: Enabling various hardware-dependent features
- `home`: HomeManager configurations; contains user packages, dotfiles, and
  other configurations
  - `home/<name>/`: HomeManager configurations for a specific program, or for
    a group of related programs
- `i18n`: Internationalization settings and console settings, like locale and
  keymap
- `networking`: Network and wireless settings
- `nix`: Nix and nixpkgs configurations
  - `nix/overlays/`: Overlays for overriding packages in nixpkgs or introducing
    new ones
- `pkgs`: System-wide packages, like bash and vim, and their configurations
  - `pkgs/scripts.nix`: Scripts to be available globally
- `security`: Security-related configurations
- `services`: Various daemons, user-session programs, and other services
- `xserver`: Configurations for X, including the window manager, compositor,
  and more
- `time`: Time and timezone settings
- `users`: User and group configurations
- `common`: Nix expressions used throughout the rest of the config
- `virtualisation`: Manage virtualization services like Docker and VirtualBox

Additionally, other directories don't contain configuration, but Nix-related
helpers:

- `iso`: Derivations to build NixOS isos

The configuration also assumes some untracked files other than
`hardware-configuration.nix`:

- `networking/interfaces.txt`: A file with networking interfaces for which to
  enable DHCP. These are generally different for each computer.

These can be automatically created with `setup.sh`.

