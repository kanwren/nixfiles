{ pkgs, self, lib, ... }:

let
  cdAliases = builtins.listToAttrs (builtins.map
    (n: {
      name = ".${toString n}";
      value = "cd ${builtins.concatStringsSep "/" (builtins.genList (_: "..") n)}";
    })
    (lib.lists.range 1 9));
in
{
  environment = {
    # set login shell to fish
    loginShell = "${pkgs.fish}/bin/fish -l";
    variables.SHELL = "${pkgs.fish}/bin/fish";

    shellAliases = cdAliases // {
      vi = "nvim";
      vim = "nvim";
      ls = "${pkgs.eza}/bin/eza --git";
      cat = "${pkgs.bat}/bin/bat";
    };

    systemPackages = with pkgs; [
      fishPlugins.fzf-fish
      fishPlugins.foreign-env
      self.packages.${pkgs.system}.wd-fish
    ];

    variables.LC_CTYPE = "en_US.UTF-8";
  };

  programs = {
    bash = {
      enable = true;
      enableCompletion = true;
      interactiveShellInit = ''
        export PATH="$HOME/bin:''${PATH:+$PATH:}/usr/local/bin:/usr/local/sbin:/opt/local/bin:/opt/local/sbin"

        if [ `ulimit -n` -lt 8192 ]; then
          ulimit -n 8192
        fi

        set -o vi
        shopt -s checkwinsize
        shopt -s extglob
        shopt -s globstar
        shopt -s checkjobs
        shopt -s histappend
        shopt -s cmdhist
        export HISTFILE="$HOME/.bash_history"
        export HISTCONTROL=ignoreboth
        export HISTSIZE=1000000
        export HISTFILESIZE=1000000
        export HISTIGNORE="ls:cd:exit:history"

        PS1='; '
      '';
    };

    zsh = {
      enable = true;
      enableCompletion = true;
      enableFzfCompletion = true;
      enableFzfGit = true;
      enableSyntaxHighlighting = true;
      enableFzfHistory = true;

      shellInit = ''
        typeset -U path PATH

        if [ `ulimit -n` -lt 8192 ]; then
          ulimit -n 8192
        fi

        export PATH="$HOME/bin:''${PATH:+$PATH:}/usr/local/bin:/usr/local/sbin:/opt/local/bin:/opt/local/sbin"
      '';

      loginShellInit = ''
        # set up homebrew env
        if command -v /opt/homebrew/bin/brew &>/dev/null; then
          eval "$(/opt/homebrew/bin/brew shellenv)"

          export HOMEBREW_NO_ANALYTICS=1

          if command -v brew &>/dev/null; then
            # Used for C pre-processor/#include. Confirm paths with `clang -x c -v -E /dev/null`
            export CPATH="$(brew --prefix)/include''${CPATH:+:$CPATH}"

            # Used by linker. Confirm paths with `clang -Xlinker -v`
            export LIBRARY_PATH="$(brew --prefix)/lib''${LIBRARY_PATH:+:$LIBRARY_PATH}"
          fi
        fi
      '';

      interactiveShellInit = ''
        # syntax highlighting (pkgs.zsh-syntax-highlighting sourced by enableSyntaxHighlighting)
        export ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets cursor)

        # autosuggest
        source ${pkgs.zsh-autosuggestions}/share/zsh-autosuggestions/zsh-autosuggestions.zsh
        export ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=8"
        export ZSH_AUTOSUGGEST_STRATEGY=(history)

        if command -v brew &>/dev/null; then
          FPATH="$(brew --prefix)/share/zsh/site-functions''${FPATH:+:''${FPATH}}"
        fi

        # This is loaded by enableCompletion, but it's done after interactiveShellInit
        autoload -Uz compinit && compinit

        # direnv
        emulate zsh -c "$(${pkgs.direnv}/bin/direnv export zsh)"
        emulate zsh -c "$(${pkgs.direnv}/bin/direnv hook zsh)"

        # h
        eval "$(${pkgs.h}/bin/h --setup ~/Development/code)"
        eval "$(${pkgs.h}/bin/up --setup)"

        # history
        export HISTORY_IGNORE='([bf]g *|cd( *)#|.[.123456789]|l[alsh]#( *)#|less *|(nvim|vim#)( *)#)'
        export HISTFILE="$HOME/.zsh_history"
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
      '';

      promptInit = ''
        PROMPT='%(?.%F{green}.%F{red})%B;%b%f '
        RPROMPT=
      '';
    };

    fish = {
      enable = true;

      loginShellInit = ''
        fish_add_path -aP /usr/local/bin /usr/local/sbin /opt/local/bin

        # set up homebrew env
        if command -v /opt/homebrew/bin/brew >/dev/null 2>&1
          set -gx HOMEBREW_NO_ANALYTICS 1
          set -gx HOMEBREW_PREFIX "/opt/homebrew"
          set -gx HOMEBREW_CELLAR "/opt/homebrew/Cellar"
          set -gx HOMEBREW_REPOSITORY "/opt/homebrew"

          # Used for C pre-processor/#include. Confirm paths with `clang -x c -v -E /dev/null`
          not set -q CPATH; and set CPATH ""
          set -gx CPATH /opt/homebrew/include:"$CPATH"

          # Used by linker. Confirm paths with `clang -Xlinker -v`
          not set -q LIBRARY_PATH; and set LIBRARY_PATH ""
          set -gx LIBRARY_PATH /opt/homebrew/lib:"$LIBRARY_PATH"

          not set -q MANPATH; and set MANPATH ""
          set -gx MANPATH /opt/homebrew/share/man:"$MANPATH"

          not set -q INFOPATH; and set INFOPATH ""
          set -gx INFOPATH /opt/homebrew/share/info:"$INFOPATH"

          fish_add_path -p /opt/homebrew/bin /opt/homebrew/sbin
        end

        # give NixOS paths priority over brew and system paths
        fish_add_path -p $HOME/.nix-profile/bin /etc/profiles/per-user/$USER/bin /nix/var/nix/profiles/default/bin /run/current-system/sw/bin
      '';

      promptInit = ''
        function fish_prompt
            if test $status = 0
                set_color --bold green
            else
                set_color --bold red
            end
            printf '; '
            set_color normal
        end

        # disable vi mode prompt indicator
        function fish_mode_prompt; end

        set -gx fish_greeting ""
      '';

      interactiveShellInit = ''
        fish_add_path -p "$HOME/bin"
        direnv hook fish | source

        function h
            if set -l target (command h --resolve "$HOME/Development/code" $argv)
                if [ $target != (pwd) ]
                    echo $target
                    cd $target
                end
            else
                return $status
            end
        end

        function up
            if set -l target (command up $argv)
                if [ $target != (pwd) ]
                    echo $target
                    cd $target
                end
            else
                return $status
            end
        end
      '';
    };
  };
}
