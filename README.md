# nixos-configs

My NixOS configurations. Configs are split up across multiple directories:
- `boot`: Configuration of the bootloader and boot process
- `hardware`: Enabling various hardware-dependent features
- `home`: HomeManager configurations; contains user packages, dotfiles, and
    other configurations
- `i18n`: Internationalization settings like locale and keymap
- `networking`: Network and wireless settings
- `nix`: Nixpkgs configuration and overlays
- `pkgs`: System-wide packages, like bash and vim, and their configurations
- `security`: Security-related configurations
- `services`: Various daemons, user-session programs, and other services,
    including window manager and compositor configs
- `time`: Time and timezone settings
- `users`: User and group configurations
- `utils`: Defines various utility functions into `config.lib.utils`
- `virtualisation`: Manage virutalization services like Docker and VirtualBox

Additionally, other directories don't contain configuration, but Nix-related
helpers:
- `iso`: Derivations to build NixOS isos

