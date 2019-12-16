{ config, pkgs, ... }:

let
  nord-tmux = pkgs.tmuxPlugins.mkDerivation {
    pluginName = "nord-tmux";
    src = pkgs.fetchFromGitHub {
      owner = "arcticicestudio";
      repo = "nord-tmux";
      rev = "25c64f5fc4ff716fae7256d9a8f6af4724644edc";
      sha256 = "14xhh49izvjw4ycwq5gx4if7a0bcnvgsf3irywc3qps6jjcf5ymk";
    };
    rtpFilePath = "nord.tmux";
  };
in
{
  home-manager.users.nprin = {

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

  };
}

