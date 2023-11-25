# nixfiles

[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)

My NixOS configurations and other Nix files :snowflake:

### Overview

- [`flake.nix`](flake.nix): Flakes for each configuration, outputs for libs/modules/packages, and a dev shell for working with secrets.
- [`modules/`](modules/): Custom NixOS modules; see the [`mixins`](#mixins) section
- [`hosts/`](hosts/): Configuration for each of my NixOS machines
- [`hm-modules/`](hm-modules/): Custom home-manager modules
- [`darwin-modules/`](darwin-modules/): Custom nix-darwin modules
- [`pkgs/`](pkgs/): Custom-built derivations exported from flake
- [`installer/`](installer/): Minimal custom installer configuration using [nixos-generators](https://github.com/nix-community/nixos-generators); see the [`installer`](#installer) section
- [`overlays/`](overlays/): Nixpkgs overlays for overriding or adding packages
- [`templates/`](templates/): Various templates that can be used with the `nix flake init/new` subcommands

### Hosts

- [`hecate`](hosts/hecate/): Main laptop
- [`caspar`](hosts/caspar/): M1 Mac

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

Alternatively, some installers are exported by default:

```
$ nix build 'github:kanwren/nixfiles#packages.x86_64-linux.install-iso'
$ nix build 'github:kanwren/nixfiles#packages.aarch64-linux.sd-aarch64-installer'
```

### Manual setup

Some things cannot be set up automatically, especially some of the Catppuccin
theming. Here are some things that may need to be done manually:

- BetterDiscord installation: run `betterdiscordctl install`
- Obsidian theming: follow the instructions [here](https://github.com/catppuccin/obsidian#obsidian-theme-store)
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
- Wallpapers: <https://github.com/catppuccin/wallpapers>
  - Update wallpaper with `betterlockscreen -u ...` and `betterlockscreen -w`

