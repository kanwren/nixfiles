# nixfiles

[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)

My NixOS configurations and other Nix files :snowflake:

### Overview

- [`flake.nix`](flake.nix): Flakes for each configuration, outputs for libs/modules/packages, and a dev shell for working with secrets.
- [`modules/`](modules/): Custom NixOS modules; see the [`mixins`](#mixins) section
- [`hosts/`](hosts/): Configuration for each of my NixOS machines
- [`hm-modules/`](hm-modules/): Custom home-manager modules
- [`pkgs/`](pkgs/): Custom-built derivations exported from flake
- [`lib/`](lib/): Custom library functions used throughout the configs and exported from flake
- [`installer/`](installer/): Minimal custom installer configuration using [nixos-generators](https://github.com/nix-community/nixos-generators); see the [`installer`](#installer) section
- [`overlays/`](overlays/): Nixpkgs overlays for overriding or adding packages
- [`secrets/`](secrets/): Secrets are managed using [sops-nix](https://github.com/Mic92/sops-nix)
- [`templates/`](templates/): Various templates that can be used with the `nix flake init/new` subcommands

### Hosts

- [`hecate`](hosts/hecate/): My main laptop
- [`homepi`](hosts/homepi/): My Raspberry Pi running [Home Assistant](https://www.home-assistant.io/)

#### Mixins

Most of the code for building configurations is split into mixins, which are
NixOS modules that configure part of a system according to my preferences.
Mixins can be mixed-and-matched and composed together to create the base config
for a specific system.

- [`base/`](modules/base): The common base of all of my systems, with essential packages, services, and settings
- [`desktop/`](modules/desktop): Different mixins for creating a development workstation on a (usually graphical) computer
  - [`desktop/x`](modules/desktop/x): Mixins for display managers, window managers, and desktop environments
- [`users/`](modules/users): Per-user system user settings and [home-manager](https://github.com/nix-community/home-manager/) configurations

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

### Manual setup

Some things cannot be set up automatically, especially some of the Catppuccin
theming. Here are some things that may need to be done manually:

- BetterDiscord installation: run `betterdiscordctl install`
- Browser stuff:
  - Userchrome stuff (`~/.mozilla/firefox/<profile>/chrome/userChrome.css`):
    - Hide tab bar: `#tabbrowser-tabs { visibility: collapse !important; }`
    - Hide TST bar: `#TabsToolbar { visibility: collapse !important; } #sidebar-header { display: none; }`
  - Firefox extensions:
    - Themes:
      - [catppuccin-mocha-lavender](https://addons.mozilla.org/en-US/firefox/addon/catppuccin-mocha-lavender/)
    - Extensions:
      - Privacy:
        - [uBlock Origin](https://addons.mozilla.org/en-US/firefox/addon/ublock-origin/)
        - [FoxyProxy](https://addons.mozilla.org/en-US/firefox/addon/foxyproxy-standard/)
      - Integrations:
        - [Bitwarden](https://addons.mozilla.org/en-US/firefox/addon/bitwarden-password-manager/)
      - Interface/navigation:
        - [Vim Vixen](https://addons.mozilla.org/en-US/firefox/addon/vim-vixen/)
      - Workflow:
        - [Tab session manager](https://addons.mozilla.org/en-US/firefox/addon/tab-session-manager/)
        - [Tree style tab](https://addons.mozilla.org/en-US/firefox/addon/tree-style-tab/)
          - [TST Indent Line](https://addons.mozilla.org/en-US/firefox/addon/tst-indent-line/)
          - [TST Lock tree collapsed](https://addons.mozilla.org/en-US/firefox/addon/tst-lock-tree-collapsed/)
          - [TST Tab Drag Handle](https://addons.mozilla.org/en-US/firefox/addon/tst-tab-drag-handle/)
          - [TST-MiddleClick](https://addons.mozilla.org/en-US/firefox/addon/tst-middleclick/)
          - [Move unloaded tabs for Tree Style Tab](https://addons.mozilla.org/en-US/firefox/addon/move-unloaded-tabs-for-tst/)
          - [Bookmark Tree for Tree Style Tab](https://addons.mozilla.org/en-US/firefox/addon/bookmark-tree-for-tst/)
      - Site enhancements:
        - [Stylus](https://addons.mozilla.org/en-US/firefox/addon/styl-us/)
          - [Catppuccin for GitHub](https://github.com/catppuccin/github/raw/main/catppuccin.user.css)
          - [Youtube-Catppuccin Mocha](https://github.com/catppuccin/YouTube/raw/main/src/YouTubeCatppuccinMocha.user.css)
          - [Catppuccin for DDG](https://github.com/catppuccin/duckduckgo)
        - [YouTube Windowed FullScreen](https://addons.mozilla.org/en-US/firefox/addon/youtube-window-fullscreen/)
      - Misc:
        - [Tabby cat](https://addons.mozilla.org/en-US/firefox/addon/tabby-cat-friend/)
- Wallpapers: https://github.com/catppuccin/wallpapers
  - Update wallpaper with `betterlockscreen -u ...` and `betterlockscreen -w`
