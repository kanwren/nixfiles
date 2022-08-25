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
        # syntax highlighting (pkgs.zsh-syntax-highlighting sourced by enableSyntaxHighlighting)
        export ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets cursor)

        # autosuggest
        source ${pkgs.zsh-autosuggestions}/share/zsh-autosuggestions/zsh-autosuggestions.zsh
        export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=8"
        export ZSH_AUTOSUGGEST_STRATEGY=(history)

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
          "golang"
          "kubectl"
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
      enable = true;
      enableScriptingAddition = false;
      package = pkgs.yabai;
      config = {
        layout = "bsp";
        split_ratio = 0.50;
        window_placement = "second_child";
        window_gap = 10;
        mouse_modifier = "fn";
        mouse_action1 = "move";
        mouse_action2 = "resize";
        mouse_drop_action = "swap";
      };
      extraConfig =
        let
          don'tManage = [
            "System Preferences"
            "System Information"
            "Calculator"
            "Installer"
          ];
        in
        ''
          ${
            # TODO: may or may not want to add regex anchors
            builtins.concatStringsSep "\n" (builtins.map (x: ''
              yabai -m rule --add app="${x}" manage=off
            '') don'tManage)
          }
        '';
    };
    skhd = {
      # https://github.com/foreverd34d/.dotfiles/blob/master/skhd/skhdrc
      enable = true;
      # note: not all of this works without disabling SIP
      skhdConfig = with pkgs; ''
        ctrl + alt - t : yabai -m config layout bsp
        ctrl + alt - s : yabai -m config layout stack
        ctrl + alt - f : yabai -m config layout float

        alt - h : yabai -m window --focus west
        alt - j : yabai -m window --focus south
        alt - k : yabai -m window --focus north
        alt - l : yabai -m window --focus east

        alt - n : yabai -m query --spaces --space | ${jq}/bin/jq -re ".index" | xargs -I{} yabai -m query --windows --space {} | ${jq}/bin/jq -sre "add | map(select(.minimized != 1)) | sort_by(.display, .frame.y, .frame.x, .id) | reverse | nth(index(map(select(.focused == 1))) - 1).id" | xargs -I{} yabai -m window --focus {}
        alt - p : yabai -m query --spaces --space | ${jq}/bin/jq -re ".index" | xargs -I{} yabai -m query --windows --space {} | ${jq}/bin/jq -sre "add | map(select(.minimized != 1)) | sort_by(.display, .frame.y, .frame.y, .id) | nth(index(map(select(.focused == 1))) - 1).id" | xargs -I{} yabai -m window --focus {}
        # alt - n : yabai -m window --focus stack.next
        # alt - p : yabai -m window --focus stack.prev

        shift + alt - h : yabai -m window --swap west
        shift + alt - j : yabai -m window --swap south
        shift + alt - k : yabai -m window --swap north
        shift + alt - l : yabai -m window --swap east

        # < / >
        alt - 0x2B : yabai -m space --focus prev
        alt - 0x2F : yabai -m space --focus next
        alt - 1 : yabai -m space --focus 1
        alt - 2 : yabai -m space --focus 2
        alt - 3 : yabai -m space --focus 3
        alt - 4 : yabai -m space --focus 4
        alt - 5 : yabai -m space --focus 5
        alt - 6 : yabai -m space --focus 6
        alt - 7 : yabai -m space --focus 7
        alt - 8 : yabai -m space --focus 8

        shift + alt - 0x2B : yabai -m window --space prev
        shift + alt - 0x2F : yabai -m window --space next
        shift + alt - 1 : yabai -m window --space 1
        shift + alt - 2 : yabai -m window --space 2
        shift + alt - 3 : yabai -m window --space 3
        shift + alt - 4 : yabai -m window --space 4
        shift + alt - 5 : yabai -m window --space 5
        shift + alt - 6 : yabai -m window --space 6
        shift + alt - 7 : yabai -m window --space 7
        shift + alt - 8 : yabai -m window --space 8

        ctrl + shift + alt - 0x2B : yabai -m window --space prev; yabai -m space --focus prev
        ctrl + shift + alt - 0x2F : yabai -m window --space next; yabai -m space --focus next
        ctrl + shift + alt - 1 : yabai -m window --space 1; yabai -m space --focus 1
        ctrl + shift + alt - 2 : yabai -m window --space 2; yabai -m space --focus 2
        ctrl + shift + alt - 3 : yabai -m window --space 3; yabai -m space --focus 3
        ctrl + shift + alt - 4 : yabai -m window --space 4; yabai -m space --focus 4
        ctrl + shift + alt - 5 : yabai -m window --space 5; yabai -m space --focus 5
        ctrl + shift + alt - 6 : yabai -m window --space 6; yabai -m space --focus 6
        ctrl + shift + alt - 7 : yabai -m window --space 7; yabai -m space --focus 7
        ctrl + shift + alt - 8 : yabai -m window --space 8; yabai -m space --focus 8

        # TODO: displays!

        ctrl + alt - n : yabai -m space --create && yabai -m space --focus next
        ctrl + alt - x : yabai -m space --destroy
        ctrl + alt - g : yabai -m space --toggle padding; yabai -m space --toggle gap
        ctrl + alt - b : yabai -m space --balance
        ctrl + alt - q : yabai -m window --close
        alt - r : yabai -m space --rotate 90
        alt - y : yabai -m space --mirror y-axis
        alt - u : yabai -m space --mirror x-axis
        ctrl + alt - space : yabai -m window --toggle float
        alt - f : yabai -m window --toggle zoom-fullscreen
        shift + alt - f : yabai -m window --toggle native-fullscreen
      '';
    };
  };

  fonts = {
    fonts = with pkgs; [
      (nerdfonts.override { fonts = [ "FiraMono" "FiraCode" ]; })
    ];
  };
}
