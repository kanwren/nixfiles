{ pkgs, self, lib, ... }:

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
  };
}
