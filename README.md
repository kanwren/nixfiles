# nixfiles

[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)

My NixOS configurations and other Nix files :snowflake:

## Hosts

- [`hecate`](hosts/hecate/)
- [`caspar`](hosts/caspar/)

## `installers`

`installers/` contains a custom installer configuration, built with [nixos-generators](https://github.com/nix-community/nixos-generators).
The `justfile` contains a recipe for building installers:

```
# x86_64-linux installer iso
$ just build-nixos-installer x86_64-linux ./installers/configuration.nix

# aarch64-linux installer sd image (requires 'boot.binfmt.emulatedSystems = [ "aarch64-linux" ];')
$ just build-nixos-installer aarch64-linux ./installers/configuration.nix
```

## Manual setup

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
          - [TST Lock Tree Collapsed](https://addons.mozilla.org/en-US/firefox/addon/tst-lock-tree-collapsed/)
          - [TST More Tree Commands](https://addons.mozilla.org/en-US/firefox/addon/tst-more-tree-commands/)
          - [TST Tab Drag Handle](https://addons.mozilla.org/en-US/firefox/addon/tst-tab-drag-handle/)
          - [TST-MiddleClick](https://addons.mozilla.org/en-US/firefox/addon/tst-middleclick/)
          - [Tab Unloader for Tree Style Tab](https://addons.mozilla.org/en-US/firefox/addon/tab-unload-for-tree-style-tab/)
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

