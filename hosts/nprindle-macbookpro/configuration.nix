{ pkgs, self, lib, ... }:

let
  makeOmzConfig =
    { package ? pkgs.oh-my-zsh
    , plugins ? [ ]
    , customPkgs ? [ ]
    }:
    let
      mkLinkFarmEntry = name: dir:
        let
          env = pkgs.buildEnv {
            name = "zsh-${name}-env";
            paths = customPkgs;
            pathsToLink = "/share/zsh/${dir}";
          };
        in
        { inherit name; path = "${env}/share/zsh/${dir}"; };
    in
    ''
      export ZSH="${package}/share/oh-my-zsh"
      export plugins=(${builtins.concatStringsSep " " plugins})
      export ZSH_CUSTOM="${pkgs.linkFarm "oh-my-zsh-custom" [
        (mkLinkFarmEntry "themes" "themes")
        (mkLinkFarmEntry "completions" "site-functions")
        (mkLinkFarmEntry "plugins" "plugins")
      ]}"
      export ZSH_CACHE_DIR="$HOME/.cache/oh-my-zsh"
      if [[ ! -d "$ZSH_CACHE_DIR" ]]; then
        mkdir -p "$ZSH_CACHE_DIR"
      fi

      # This has to be sourced in the user's ~/.zshrc, due to invasive shell setup that runs at the beginning of ~/.zshrc
      # source $ZSH/oh-my-zsh.sh
    '';
in

{
  imports = [
    ./nix.nix
  ];

  environment.systemPackages = with pkgs; [
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
  ];

  environment.variables = {
    EDITOR = "nvim";
  };

  environment.shellAliases =
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
      ls = "${pkgs.exa}/bin/exa --git";
      cat = "${pkgs.bat}/bin/bat";
    };

  programs.zsh = {
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

      ${makeOmzConfig {
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
      }}

      # TODO: setup starship
    '';
  };

  programs = {
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
