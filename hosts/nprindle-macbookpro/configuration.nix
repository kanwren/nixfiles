{ pkgs, self, lib, ... }:

{
  imports = [
    ./nix.nix
    ./oh-my-zsh.nix
  ];

  system = {
    keyboard = {
      enableKeyMapping = true;
      userKeyMapping =
        let
          escapeKey = 30064771129;
          capsLockKey = 30064771113;
          remap = from: to: { HIDKeyboardModifierMappingSrc = from; HIDKeyboardModifierMappingDst = to; };
        in
        [
          (remap escapeKey capsLockKey)
          (remap capsLockKey escapeKey)
        ];
    };
  };

  environment = {
    systemPackages = with pkgs; [
      btop
      ripgrep
      direnv
      h
      exa
      bat
      fzf
      jq
      fd
      sd
      wget
      tldr
      cht-sh
      tree
      unar
      watch
    ];

    variables = {
      EDITOR = "nvim";
    };

    shellAliases =
      let
        repeat = n: x: builtins.genList (_: x) n;
        mkCdAlias = n: {
          name = ".${toString n}";
          value = "cd ${builtins.concatStringsSep "/" (repeat n "..")}";
        };
        cdAliases = { ".." = "cd .."; } // builtins.listToAttrs (builtins.map mkCdAlias (lib.lists.range 1 9));
      in
      cdAliases // {
        vi = "nvim";
        vim = "nvim";
        cat = "${pkgs.bat}/bin/bat";
        ls = "${pkgs.exa}/bin/exa --git";
        l = "${pkgs.exa}/bin/exa --git -lah";
        la = "${pkgs.exa}/bin/exa --git -lAh";
        ll = "${pkgs.exa}/bin/exa --git -lh";
        lsa = "${pkgs.exa}/bin/exa --git -lh";
      };
  };

  programs = {
    bash = {
      enable = true;
      enableCompletion = true;
      interactiveShellInit = ''
        set -o vi
        shopt -s checkwinsize
        shopt -s extglob
        shopt -s globstar
        shopt -s checkjobs
        shopt -s histappend
        shopt -s cmdhist
        HISTCONTROL=ignoreboth
        HISTSIZE=10000
        HISTFILESIZE=10000
        HISTIGNORE="ls:cd:exit:history"
      '';
    };
    zsh = {
      enable = true;
      enableCompletion = true;
      enableFzfCompletion = true;
      enableFzfGit = true;
      enableSyntaxHighlighting = true;
      enableFzfHistory = true;
      promptInit = "";
      interactiveShellInit = ''
        # This is loaded by enableCompletion, but it's done after interactiveShellInit
        autoload -Uz compinit && compinit

        # direnv
        emulate zsh -c "$(${pkgs.direnv}/bin/direnv export zsh)"
        emulate zsh -c "$(${pkgs.direnv}/bin/direnv hook zsh)"

        # h
        eval "$(${pkgs.h}/bin/h --setup ~/code)"
        eval "$(${pkgs.h}/bin/up --setup)"

        # history
        export HISTORY_IGNORE='([bf]g *|cd( *)#|.[.123456789]|l[alsh]#( *)#|less *|(nvim|vim#)( *)#)'
        export HISTSIZE=1000000
        export SAVEHIST=1000000
        setopt APPEND_HISTORY
        setopt INC_APPEND_HISTORY
        setopt HIST_IGNORE_ALL_DUPS
        setopt HIST_IGNORE_DUPS

        setopt autocd extendedglob
        unsetopt beep

        bindkey -v
        export KEYTIMEOUT=1

        export FZF_BASE="${pkgs.fzf}/share/fzf"

        # See github:spwhitt/nix-zsh-completions/issues/32
        function _nix() {
          local ifs_bk="$IFS"
          local input=("''${(Q)words[@]}")
          IFS=$'\n'$'\t'
          local res=($(NIX_GET_COMPLETIONS=$((CURRENT - 1)) "$input[@]"))
          IFS="$ifs_bk"
          local tpe="$res[1]"
          local suggestions=(''${res:1})
          if [[ "$tpe" == filenames ]]; then
            compadd -fa suggestions
          else
            compadd -a suggestions
          fi
        }
        compdef _nix nix

        # Fix fzf not being loaded with zsh-vi-mode
        # See https://github.com/jeffreytse/zsh-vi-mode#execute-extra-commands
        function zvm_after_init() {
          source "${pkgs.oh-my-zsh}/share/oh-my-zsh/plugins/fzf/fzf.plugin.zsh"
        }

        # TODO: setup starship
      '';

      ohMyZsh = {
        enable = true;
        plugins = [
          "bazel"
          "colored-man-pages"
          "command-not-found"
          "docker"
          "fzf"
          "git"
          "last-working-dir"
          "nix-shell"
          "ripgrep"
          "safe-paste"
          "wd"
          "z"
          "zsh-vi-mode"
        ];
        customPkgs = [
          (pkgs.runCommand "zsh-nix-shell" { } ''
            mkdir -p "$out"/share/zsh/plugins
            cp -r ${pkgs.zsh-nix-shell}/share/zsh-nix-shell "$out"/share/zsh/plugins/nix-shell
          '')
          (pkgs.runCommand "zsh-vi-mode" { } ''
            mkdir -p "$out"/share/zsh/plugins
            cp -r ${pkgs.zsh-vi-mode}/share/zsh-vi-mode "$out"/share/zsh/plugins/zsh-vi-mode
          '')
        ];

        # $ZSH/oh-my-zsh.sh should be source in ~/.zshrc, due to invasive shell
        # setup that runs at the beginning of ~/.zshrc. it's good to also
        # protect environment.shellAliases from being overwritten by wrapping
        # the load in
        # ```
        # save_aliases=$(alias -L)
        # ...
        # eval $save_aliases; unset save_aliases
        # ```
        doLoad = false;
      };
    };

    tmux = {
      enable = true;
      enableSensible = true;
      enableMouse = true;
      enableVim = true;
      enableFzf = true;

      extraConfig = ''
        source-file ${self.packages.${pkgs.system}.catppuccin-tmux}/catppuccin.conf
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

  services = {
    yabai = {
      enable = false;
      package = pkgs.yabai;
      # catppuccin example: https://github.com/foreverd34d/.dotfiles/blob/master/yabai/yabairc
    };
    skhd.enable = false; # https://github.com/foreverd34d/.dotfiles/blob/master/skhd/skhdrc
    spacebar = {
      enable = false;
      package = pkgs.spacebar;
      # catppuccin example: https://github.com/foreverd34d/.dotfiles/blob/master/spacebar/spacebarrc
      config = {
        position = "top";
        display = "main";
        height = 26;
        title = "on";
        spaces = "on";
        clock = "on";
        power = "on";
        padding_left = 20;
        padding_right = 20;
        spacing_left = 25;
        spacing_right = 15;
        text_font = ''"FiraCode Nerd Font:Regular:12.0"'';
        icon_font = ''"FiraCode Nerd Font:Regular:12.0"'';
        background_color = "0xff202020";
        foreground_color = "0xffa8a8a8";
        power_icon_color = "0xffcd950c";
        battery_icon_color = "0xffd75f5f";
        dnd_icon_color = "0xffa8a8a8";
        clock_icon_color = "0xffa8a8a8";
        power_icon_strip = " ";
        space_icon = "•";
        space_icon_strip = "1 2 3 4 5 6 7 8 9 10";
        spaces_for_all_displays = "on";
        display_separator = "on";
        display_separator_icon = "";
        space_icon_color = "0xff458588";
        space_icon_color_secondary = "0xff78c4d4";
        space_icon_color_tertiary = "0xfffff9b0";
        clock_icon = "";
        dnd_icon = "";
        clock_format = ''"%d/%m/%y %R"'';
        right_shell = "on";
        right_shell_icon = "";
        right_shell_command = "whoami";
      };
    };
  };

  fonts = {
    fonts = with pkgs; [
      (nerdfonts.override { fonts = [ "FiraMono" "FiraCode" ]; })
    ];
  };
}
