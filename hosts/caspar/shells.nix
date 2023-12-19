{ pkgs, self, lib, ... }:

let
  commonInit = ''
    # ~/bin
    export PATH="$HOME/bin''${PATH:+:$PATH}"

    # other system paths /opt/local/bin
    export PATH="$HOME/bin:''${PATH:+$PATH:}/usr/local/bin:/usr/local/sbin:/opt/local/bin:/opt/local/sbin"
  '';

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
      g = "git";
      k = "kubectl";
      cat = "${pkgs.bat}/bin/bat";
      ls = "${pkgs.eza}/bin/eza --git";
      l = "${pkgs.eza}/bin/eza --git -lah";
      la = "${pkgs.eza}/bin/eza --git -lah";
      ll = "${pkgs.eza}/bin/eza --git -lh";
      lsa = "${pkgs.eza}/bin/eza --git -lh";
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
        ${commonInit}

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

        ${commonInit}
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

        export GPG_TTY="$(tty)"
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

        # prompt
        PROMPT='%(?.%F{green}.%F{red})%B;%b%f '
        RPROMPT=
      '';

      ohMyZsh = {
        enable = true;
        plugins = [
          "bazel"
          "colored-man-pages"
          "docker"
          "fzf"
          "git"
          "gh"
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
      };
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

        set -gx GPG_TTY (tty)
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
