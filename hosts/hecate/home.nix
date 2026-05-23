{
  pkgs,
  lib,
  config,
  ...
}:

{
  home = {
    stateVersion = "25.05";

    sessionPath = [ "$HOME/bin" ];

    sessionVariables = {
      EDITOR = "nvim";
      VISUAL = "nvim";
    };

    packages = [
      pkgs.adw-gtk3 # fixes GTK theming, per https://github.com/noctalia-dev/noctalia-shell/issues/1713
      pkgs.bitwarden-desktop
      pkgs.dolphin-emu
      pkgs.joycond-cemuhook
      pkgs.libreoffice
      pkgs.mcrcon
      pkgs.mkvtoolnix
      pkgs.python314Packages.subliminal
      pkgs.qalculate-gtk
      pkgs.qt6Packages.qt6ct # fixes some QT theming, such as Dolphin
      pkgs.signal-desktop
      pkgs.transmission_4-gtk
      pkgs.trash-cli
      pkgs.xwayland-satellite
    ];
  };

  mixins = {
    bash.enable = true;
    btop.enable = true;
    catppuccin.enable = true;
    direnv.enable = true;
    discord.enable = true;
    fish.enable = true;
    gh.enable = true;
    git.enable = true;
    gpg-agent.enable = true;
    h.enable = true;
    jq.enable = true;
    jujutsu.enable = true;
    nix.enable = true;
    yazi.enable = true;
    zathura.enable = true;
    zellij.enable = true;
    zoxide.enable = true;
  };

  programs = {
    chromium.enable = true;

    firefox = {
      enable = true;
      configPath = "${config.xdg.configHome}/mozilla/firefox";
    };

    ghostty = {
      enable = true;
      systemd.enable = true;
      clearDefaultKeybinds = true; # prefer to use the window manager to splits/tabs/etc.
      settings = {
        font-size = 11.0;
        font-family = "FiraCode Nerd Font Mono";
        shell-integration-features = lib.concatStringsSep "," [
          "cursor"
          "sudo"
          "title"
          "ssh-env"
          "ssh-terminfo"
          "path"
        ];
        keybind = [
          "ctrl+shift+p=toggle_command_palette"
          "ctrl+shift+n=new_window"

          "ctrl+==increase_font_size:1"
          "ctrl++=increase_font_size:1"
          "ctrl+-=decrease_font_size:1"
          "ctrl+0=reset_font_size"

          "ctrl+shift+f=start_search"
          "ctrl+shift+c=copy_to_clipboard:mixed"
          "ctrl+shift+v=paste_from_clipboard"
          "ctrl+shift+j=write_screen_file:paste,plain"

          "shift+page_up=scroll_page_up"
          "shift+page_down=scroll_page_down"
          "shift+home=scroll_to_top"
          "shift+end=scroll_to_bottom"
          "ctrl+shift+page_up=jump_to_prompt:-1"
          "ctrl+shift+page_down=jump_to_prompt:1"
        ];
      };
    };

    jujutsu.settings = {
      user.email = "nicole@wren.systems";
      signing = {
        behavior = "own";
        backend = "gpg";
        key = "002937658A2F43138C3B267E339C3A5C672CEA46";
      };
    };

    mpv.enable = true;

    noctalia-shell = {
      enable = true;
      settings = {
        # TODO
      };
    };

    obsidian.enable = true;

    rclone.enable = true;

    yt-dlp.enable = true;
  };
}
