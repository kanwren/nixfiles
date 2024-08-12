{ config, lib, ... }:

let
  cfg = config.mixins.tmux;
in
{
  options.mixins.tmux.enable = lib.mkOption {
    type = lib.types.bool;
    default = config.mixins.enable;
    description = "Whether to enable the tmux mixin";
  };

  config = lib.mkIf cfg.enable {
    programs.tmux = {
      enable = true;
      sensibleOnTop = true;
      terminal = "screen-256color";
      baseIndex = 1;
      escapeTime = 0;
      disableConfirmationPrompt = true;
      clock24 = true;
      keyMode = "vi";
      historyLimit = 10000;
      extraConfig = ''
        set-option -sa terminal-overrides ',xterm-kitty:RGB'

        # Set prefix to M-Space (shortcut doesn't support M-)
        unbind C-b
        set -g prefix M-Space
        bind-key M-Space send-prefix
        set -g prefix2 M-c

        set-window-option -g automatic-rename on

        # Activity monitoring
        setw -g monitor-activity on
        set -g visual-activity off
        set -g display-time 4000

        # hjkl pane movements
        bind h select-pane -L
        bind j select-pane -D
        bind k select-pane -U
        bind l select-pane -R

        # Flip orientation of current pane with the pane before it
        bind M-J move-pane -t '.-'
        bind M-L move-pane -h -t '.-'

        # Clipboard integration with xsel
        bind -T copy-mode-vi Enter send-keys -X copy-pipe-and-cancel 'xsel -ib'
      '';
    };
  };
}

