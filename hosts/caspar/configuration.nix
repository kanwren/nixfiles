{ pkgs, self, lib, ... }:

{
  imports = [
    ./nix.nix
    ./shells.nix
  ];

  system = {
    keyboard = {
      enableKeyMapping = true;
      userKeyMapping =
        let
          escapeKey = 30064771113;
          capsLockKey = 30064771129;
          remap = from: to: { HIDKeyboardModifierMappingSrc = from; HIDKeyboardModifierMappingDst = to; };
        in
        [
          # (remap escapeKey capsLockKey)
          (remap capsLockKey escapeKey)
        ];
    };

    defaults = {
      dock = {
        autohide = true;
        orientation = "bottom";
        show-process-indicators = true;
        showhidden = true;
        mru-spaces = false;
      };

      finder = {
        AppleShowAllExtensions = true;
        AppleShowAllFiles = true;
        ShowStatusBar = true;
        ShowPathbar = true;
        FXEnableExtensionChangeWarning = false;
      };
    };
  };

  environment = {
    systemPackages = with pkgs; [
      cachix
      gnused # bsd sed has incompatible cli
      diffutils # bsd diff incompatibilities cause problems
      btop
      ripgrep
      direnv
      h
      exa
      bat
      fzf
      jq
      yq
      dive
      fd
      sd
      wget
      tldr
      cht-sh
      tree
      unar
      watch
      gitAndTools.gitFull
      gitAndTools.gh
      bitwarden-cli
      jo
      httpie
      kitty
      helix
      moreutils
      just
      exiftool
      rustup
    ];

    variables = {
      EDITOR = "nvim";
    };
  };

  programs = {
    tmux = {
      enable = true;
      enableSensible = true;
      enableMouse = true;
      enableVim = true;
      enableFzf = true;

      extraConfig = ''
        set -g @catppuccin_flavour mocha
        run-shell ${self.packages.${pkgs.system}.catppuccin-tmux}/catppuccin.tmux

        set -g default-shell ${pkgs.zsh}/bin/zsh
        set-option -sa terminal-overrides ',xterm-kitty:RGB'
        set-window-option -g automatic-rename on
        setw -g monitor-activity on
        set -g visual-activity off
        set -g display-time 4000
        bind h select-pane -L
        bind j select-pane -D
        bind k select-pane -U
        bind l select-pane -R
        bind M-J move-pane -t '.-'
        bind M-L move-pane -h -t '.-'
      '';
    };
  };

  fonts = {
    fonts = with pkgs; [
      (nerdfonts.override { fonts = [ "FiraMono" "FiraCode" ]; })
    ];
  };

  home-manager.users.wrenn = {
    imports = [
      self.hmModules.mixins.btop
      self.hmModules.mixins.kitty
    ];

    home = {
      stateVersion = "22.11";
      sessionPath = [ "$HOME/bin" ];
    };
  };
}
