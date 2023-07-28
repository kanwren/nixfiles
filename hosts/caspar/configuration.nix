{ pkgs, self, lib, ... }:

{
  imports = [
    ./nix.nix
    ./shells.nix
  ];

  system = {
    keyboard = {
      enableKeyMapping = true;
      userKeyMapping =
        let
          escapeKey = 30064771113;
          capsLockKey = 30064771129;
          remap = from: to: { HIDKeyboardModifierMappingSrc = from; HIDKeyboardModifierMappingDst = to; };
        in
        [
          # (remap escapeKey capsLockKey)
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
      # bsd incompatibilities :<
      gnused
      gnugrep
      gnutar
      diffutils
      findutils
      patch
      bc

      cachix
      nix-index
      curl
      moreutils
      exiftool
      hyperfine
      btop
      ripgrep
      direnv
      h
      exa
      bat
      fzf
      fd
      sd
      jq
      yq-go
      wget
      tldr
      cht-sh
      shellcheck
      tree
      unar
      watch
      gitAndTools.gitFull
      gitAndTools.gh
      lazygit
      jo
      httpie
      kitty
      helix
      pandoc
      entr
      just
      qpdf
      rustup

      certstrap
      awscli2
      aws-iam-authenticator
      dive
      self.packages.${pkgs.system}.envtpl
      kubeval
      self.packages.${pkgs.system}.k8split
    ];

    variables = {
      EDITOR = "nvim";
    };
  };

  programs = {
    tmux = {
      enable = true;
      enableSensible = true;
      enableMouse = true;
      enableVim = true;
      enableFzf = true;

      extraConfig = ''
        set -g @catppuccin_flavour mocha
        run-shell ${self.packages.${pkgs.system}.catppuccin-tmux}/catppuccin.tmux

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

  fonts = {
    fonts = with pkgs; [
      (nerdfonts.override { fonts = [ "FiraMono" "FiraCode" ]; })
    ];
  };

  home-manager.users.wrenn = {
    imports = [
      self.hmModules.mixins.btop
    ];

    home = {
      stateVersion = "22.11";
      sessionPath = [ "$HOME/bin" ];
    };

    programs.kitty = {
      enable = true;
      extraConfig = ''
        ${builtins.readFile ./kitty.conf}
        include ${self.packages.${pkgs.system}.catppuccin-kitty}/mocha.conf
      '';
    };

    programs.nushell = {
      enable = true;
    };

    programs.git = {
      enable = true;
      userName = "Nicole Wren";
      userEmail = "wrenn@squareup.com";
      signing = {
        signByDefault = true;
        key = "DCC3076C9F46DFD330C3DFFDA4B4CC3C080B1C66";
      };
      includes = [
        { path = "~/.gitconfig.local"; }
      ];
      aliases = {
        s = "status";
        cane = "commit --amend --no-edit";
        amend = "commit --amend";
        pf = "push --force-with-lease";
        diffc = "diff --cached";
        conflicts = "diff --name-status --diff-filter=U";
        difff = "diff --diff-filter";
        diffno = "diff --name-only"; # diff file names only; for example, "git diffno --diff-filter=U | xargs vim"
        diffnof = "diff --name-only --diff-filter";
        rh = "reset --hard";
        ri = "rebase --interactive";
        ls = "log --oneline";
        lg = "log --graph --abbrev-commit --date=relative --pretty=format:'%C(bold blue)%h - %C(reset)%C(green)(%ar)%C(reset) - %s %C(dim)- %an%C(reset)%C(yellow)%d'";
        graph = "log --graph --oneline";
        lr = "log --left-right --graph --oneline";
        changed = "show --name-status --oneline";
        mkexec = "update-index --chmod=+x";
        root = "rev-parse --show-toplevel";
        ignored = "ls-files --others --exclude-standard";
        tag-sort = "tag --sort=v:refname";

        alias = ''! f(){ git config --get-regexp ^alias | cut -c 7- | sed -e "s/ \(.*\)/ = \1/"; }; f'';
        ignore = ''! f(){ curl -sL https://www.toptal.com/developers/gitignore/api/$@ ; }; f'';
      };
      extraConfig = {
        gist.private = true;
        color = {
          diff = "auto";
          status = "auto";
          branch = "auto";
          interactive = "auto";
        };
        log.mailmap = true;
        init.defaultBranch = "main";
        branch.autosetupmerge = true;
        filter.lfs = {
          clean = "git-lfs clean -- %f";
          smudge = "git-lfs smudge -- %f";
          process = "git-lfs filter-process";
          required = true;
        };
        rerere.enabled = 1;
        pull.ff = "only";
        push.default = "simple";
        diff = {
          renames = true;
          indentHeuristic = "on";
        };
        rebase = {
          autosquash = true;
          autostash = true;
        };
        merge = {
          summary = true;
          conflictstyle = "diff3";
        };
        mergetool = {
          prompt = false;
          keepBackup = false;
        };
      };
      ignores = [
        "*.iml"
        "*.swp"
        "*.swo"
        ".bundle"
        ".DS_Store"
        ".idea"
        ".rbx"
        "node_modules"
        "/tags"
      ];
    };

    # TODO(wrenn): this gets put into `~/.config/k9s`, but I think k9s expects
    # it in `~/Library/Application Support/k9s`.
    xdg.configFile."k9s/skin.yml".source = "${self.packages.${pkgs.system}.catppuccin-k9s}/mocha.yml";
  };
}
