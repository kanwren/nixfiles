# nixos-configs

My NixOS configurations. Configs are split up across multiple directories:

- `boot`: Configuration of the bootloader and boot process
- `hardware`: Enabling various hardware-dependent features
- `home`: HomeManager configurations; contains user packages, dotfiles, and
  other configurations
  - `home/custom/`: Miscellaneous derivations for extra user packages
  - `home/<name>/`: HomeManager configurations for a specific program, or for
    a group of related programs
- `i18n`: Internationalization settings like locale and keymap
- `networking`: Network and wireless settings
- `nix`: Nix and nixpkgs configurations
  - `nix/overlays/`: Overlays for overriding packages in nixpkgs or introducing
    new ones
- `pkgs`: System-wide packages, like bash and vim, and their configurations
  - `pkgs/scripts.nix`: Scripts to be available globally
  - `pkgs/custom/`: Miscellaneous derivations for extra global packages
- `security`: Security-related configurations
- `services`: Various daemons, user-session programs, and other services
- `xserver`: Configurations for X, including the window manager, compositor,
  and more
- `time`: Time and timezone settings
- `users`: User and group configurations
- `utils`: Defines various utility functions
- `virtualisation`: Manage virutalization services like Docker and VirtualBox

Additionally, other directories don't contain configuration, but Nix-related
helpers:

- `iso`: Derivations to build NixOS isos

These configurations assume several untracked files other than
`hardware-configuration.nix`:

- `secrets.nix`: A nested attribute set containing things like hashed user
  passwords
- `networking/interfaces.txt`: A file with networking interfaces on each line.
  DHCP will be enabled for all of these
- `networking/networks.nix`: An attr set of network names to NetworkManager
  configurations, so they're available after cleaning `/etc`

These can all be automatically created with `setup.sh`, and nix errors will
specify the expected structure.

