# Manual setup

Some things cannot be set up automatically; here's some of the things I set up
manually.

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

