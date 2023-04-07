{ pkgs, self, lib, ... }:

let
  commonInit = ''
    # ~/bin
    export PATH="$HOME/bin''${PATH:+:$PATH}"

    # other system paths /opt/local/bin
    export PATH="$HOME/bin:''${PATH:+$PATH:}/usr/local/bin:/usr/local/sbin:/opt/local/bin:/opt/local/sbin"
  '';
in
{
  environment = {
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
        x = "hx";
        cat = "${pkgs.bat}/bin/bat";
        ls = "${pkgs.exa}/bin/exa --git";
        l = "${pkgs.exa}/bin/exa --git -lah";
        la = "${pkgs.exa}/bin/exa --git -lah";
        ll = "${pkgs.exa}/bin/exa --git -lh";
        lsa = "${pkgs.exa}/bin/exa --git -lh";
      };

    variables = {
      LC_CTYPE = "en_US.UTF-8";
    };
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

        # TODO: setup starship
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
  };
}
