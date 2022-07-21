{ config, lib, pkgs, ... }:

with lib;

let
  attrsToList = as: builtins.map (name: { inherit name; value = as.${name}; }) (builtins.attrNames as);

  cfg = config.programs.btop;

  # TODO: figure out exactly what this conf format is
  generateConf =
    let
      renderBool = x: if x then "True" else "False";
      renderValue = x:
        if builtins.isBool x then
          renderBool x
        else
          builtins.toJSON x;
      renderEntry = { name, value }: "${name} = ${renderValue value}";
      renderConfig = settings: concatMapStringsSep "\n" renderEntry (attrsToList settings);
    in
    settings: pkgs.writeText "btop.conf" (renderConfig settings);
in
{
  options = {
    programs.btop = {
      enable = mkEnableOption "Whether to enable btop, a resource monitor";

      defaultTheme = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = ''
          The name of the default theme. See https://github.com/aristocratos/btop#themes for more information.
        '';
      };

      themes = mkOption {
        type = types.attrsOf types.path;
        default = { };
        description = ''
          Themes to install under ~/.config/btop/themes
        '';
      };

      config = mkOption {
        type = types.attrsOf (types.oneOf [ types.str types.int types.bool ]);
        default = { };
        description = "Configuration to be written to btop.conf";
      };
    };
  };

  config = mkIf cfg.enable {
    home.packages = [ pkgs.btop ];
    xdg.configFile =
      # for every entry name = path in cfg.themes, write out
      # "btop/themes/${name}.theme".source = path
      let
        mkTheme = { name, value }: {
          name = "btop/themes/${name}.theme";
          value = { source = value; };
        };
        themeFiles = builtins.listToAttrs (builtins.map mkTheme (attrsToList cfg.themes));
      in
      themeFiles // {
        "btop/btop.conf".source =
          let
            defaultConfig = {
              color_theme = if cfg.defaultTheme == null then "Default" else cfg.defaultTheme;
            };
          in
          generateConf (defaultConfig // cfg.config);
      };
  };
}
