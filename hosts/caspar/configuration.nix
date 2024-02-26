{ pkgs, self, lib, ... }:

{
  imports = [
    ./nix.nix
    ./shells.nix
  ];

  networking = {
    computerName = "caspar";
    hostName = "caspar.local";
    localHostName = "caspar";
  };

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
      # nix stuff
      nix-index
      nix-tree
      nix-diff

      # CLI/TUI utils
      gnugrep
      gnused
      gawk
      coreutils
      diffutils
      findutils
      patch
      netcat
      socat
      nmap
      bc
      wget
      curl
      grpcurl
      moreutils
      (lib.hiPrio (parallel-full.override { willCite = true; })) # conflicts with 'parallel' from moreutils
      tree
      ripgrep
      eza
      bat
      fzf
      fd
      rsync
      gitAndTools.gitFull
      gitAndTools.gh
      hyperfine
      direnv
      h
      tldr
      cht-sh
      watch
      entr
      ## build systems/task runners/etc.
      gnumake
      autoconf
      automake
      cmake
      bazelisk
      (runCommandNoCCLocal "bazel-bazelisk-alias" { } ''
        mkdir -p "$out/bin"
        ln -s "${bazelisk}/bin/bazelisk" "$out/bin/bazel"
      '')
      just
      ## cryptography and pki
      gnupg
      openssl
      certstrap
      certigo
      ## archival/compression
      gnutar
      gzip
      xz
      lz4
      zstd
      unar
      ## data and manipulation
      jq
      jo
      yq-go
      crudini
      sqlite
      ## TUI stuff
      tz
      btop
      ## Docker/Kubernetes
      dive
      kubectl
      kubernetes-helm
      self.packages.${pkgs.system}.envtpl
      gomplate
      ## AWS
      awscli2
      aws-iam-authenticator
      (ssm-session-manager-plugin.overrideAttrs { doCheck = false; })
      ## programming language support
      pkg-config
      go_1_22
      delve
      python3
      rustup
      self.packages.${pkgs.system}.frum
      shellcheck

      # media tools
      ffmpeg
      exiftool
      imagemagick
      pandoc
      qpdf

      # macos stuff
      pinentry_mac
    ];

    variables = {
      EDITOR = "nvim";
    };

    shellAliases = {
      cat = "bat";
      ls = "eza --git";
      vi = "nvim";
      vim = "nvim";
    };
  };

  fonts = {
    fonts = with pkgs; [
      (nerdfonts.override { fonts = [ "FiraMono" "FiraCode" ]; })
    ];
  };

  programs = {
    gnupg.agent.enable = true;
  };

  services = {
    ollama = {
      enable = true;
      logFile = "/var/tmp/ollama.log";
    };
    pueue = {
      enable = true;
      logFile = "/var/tmp/pueued.log";
    };
  };

  homebrew = {
    enable = true;
    brews = [
      "awscurl"
    ];
    casks = [
      "amethyst"
      "kitty"
      "plover"
      "talon"
      "gimp"
    ];
  };

  users.users.wrenn = {
    home = "/Users/wrenn";
  };

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;

    users.wrenn = {
      imports = [
        self.hmModules.mixins.btop
        self.hmModules.mixins.jujutsu
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
          ff = "merge --ff-only";
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
          credential.helper = "osxkeychain";
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
          ".jj"
        ];
      };

      programs.jujutsu.settings.user.email = lib.mkForce "wrenn@squareup.com";
    };
  };
}
