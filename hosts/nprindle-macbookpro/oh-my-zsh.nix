{ pkgs, lib, config, ... }:

with lib;

let
  cfg = config.programs.zsh.ohMyZsh;
in

{
  options = {
    programs.zsh.ohMyZsh = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Oh My Zsh";
      };

      package = mkOption {
        type = types.package;
        default = pkgs.oh-my-zsh;
        description = "Which package to use for Oh My Zsh";
      };

      plugins = mkOption {
        type = types.listOf types.str;
        default = [ ];
        description = "List of Oh My Zsh plugins to load";
      };

      customPkgs = mkOption {
        type = types.listOf types.package;
        default = [ ];
        description = "List of custom packages to be made available to Oh My Zsh";
      };

      cacheDir = mkOption {
        type = types.str;
        default = "$HOME/.cache/oh-my-zsh";
        description = "The directory to use for Oh My Zsh's cache";
      };

      doLoad = mkOption {
        type = types.bool;
        default = true;
        description = ''
          Whether or not to initialize the Oh My Zsh config from /etc/zshrc.
          Otherwise, `$ZSH/oh-my-zsh.sh` must be sourced in the user's ~/.zshrc.
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    programs.zsh.interactiveShellInit =
      let
        mkLinkFarmEntry = name: dir:
          let
            env = pkgs.buildEnv {
              name = "zsh-${name}-env";
              paths = cfg.customPkgs;
              pathsToLink = "/share/zsh/${dir}";
            };
          in
          { inherit name; path = "${env}/share/zsh/${dir}"; };
      in ''
        # begin oh-my-zsh configs
        export ZSH="${cfg.package}/share/oh-my-zsh"
        export plugins=(${builtins.concatStringsSep " " cfg.plugins})
        export ZSH_CUSTOM="${pkgs.linkFarm "oh-my-zsh-custom" [
          (mkLinkFarmEntry "themes" "themes")
          (mkLinkFarmEntry "completions" "site-functions")
          (mkLinkFarmEntry "plugins" "plugins")
        ]}"
        export ZSH_CACHE_DIR="${cfg.cacheDir}"
        if [[ ! -d "$ZSH_CACHE_DIR" ]]; then
          mkdir -p "$ZSH_CACHE_DIR"
        fi

        ${optionalString cfg.doLoad ''
          source $ZSH/oh-my-zsh.sh
        ''}
        # end oh-my-zsh configs
      '';
  };
}
