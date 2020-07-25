{ pkgs, ... }:

let
  sources = import ../../nix/sources.nix;
  nord-tmux = pkgs.tmuxPlugins.mkDerivation {
    pluginName = "nord-tmux";
    version = "0.3.0";
    src = sources.nord-tmux;
    rtpFilePath = "nord.tmux";
  };
in
{
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
    plugins = with pkgs; [
      {
        plugin = nord-tmux;
        extraConfig = ''
          set -g @nord_tmux_no_patched_font "1"
        '';
      }
    ];
    extraConfig = ''
      set -g default-shell ${pkgs.zsh}/bin/zsh

      # Set prefix to M-Space (shortcut doesn't support M-)
      unbind C-b
      set -g prefix M-Space
      bind-key M-Space send-prefix

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

      # Clipboard integration with xclip
      bind -T copy-mode-vi Enter send-keys -X copy-pipe-and-cancel "xclip -i -f -selection primary | xclip -i -selection clipboard"
    '';
  };
}

