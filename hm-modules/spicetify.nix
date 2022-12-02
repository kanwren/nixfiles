{ config, lib, pkgs, ... }:

let
  cfg = config.programs.spicetify;

  mkSpicetifyConfigFile =
    let
      generateConfigFile = (pkgs.formats.ini { }).generate "config-xpui.ini";
      formatConfigValues = _: v:
        if builtins.isBool v then
          if v then "1" else "0"
        else if builtins.isList v then
          builtins.concatStringsSep "|" v
        else if builtins.isInt v then
          builtins.toJSON v
        else if builtins.isAttrs v then
          builtins.mapAttrs formatConfigValues v
        else
          v;
    in
    conf: generateConfigFile (builtins.mapAttrs formatConfigValues conf);

  spicetifyConfig = {
    Setting = {
      check_spicetify_upgrade = false;
      color_scheme = cfg.colorScheme;
      inject_css = true;
      overwrite_assets = false;
      prefs_path = "PREFS_PATH";
      replace_colors = true;
      spotify_launch_flags = [ ];
      spotify_path = "SPOTIFY_PATH";
    } // lib.optionalAttrs (cfg.currentTheme != null) {
      current_theme = cfg.currentTheme;
    };

    AdditionalOptions = {
      # TODO: add support for custom apps
      # custom_apps = [ ... ];
      inherit (cfg) extensions;
      experimental_features = false;
      home_config = true;
      sidebar_config = true;
    };

    Backup = {
      version = "";
      "with" = "";
    };

    Preprocesses = {
      disable_sentry = true;
      disable_ui_logging = true;
      disable_upgrade_check = true;
      expose_apis = true;
      remove_rtl_rule = true;
    };

    Patch = { };
  };

  spotify = pkgs.spotify.overrideAttrs (oldAttrs: {
    nativeBuildInputs = (oldAttrs.nativeBuildInputs or [ ]) ++ [ pkgs.spicetify-cli ];
    postInstall = (oldAttrs.postInstall or "") + ''
      export HOME=$TMP
      spicetifyDir=$(dirname "$(spicetify-cli -c)")

      # Themes
      ${
        lib.strings.concatMapStringsSep "\n" (addon:
          ''
            # Extensions
            for f in ${addon}/share/spicetify/Extensions/*.js; do
              name=`basename $f`
              echo "Linking extension $name from $f"
              ln -s $f $spicetifyDir/Extensions/$name
            done

            # Themes
            for f in ${addon}/share/spicetify/Themes/*; do
              name=`basename $f`
              echo "Linking theme $name from $f"
              ln -s $f $spicetifyDir/Themes/$name
            done
          ''
        ) cfg.addons
      }

      touch $out/prefs
      cp -f ${mkSpicetifyConfigFile spicetifyConfig} $spicetifyDir/config-xpui.ini

      substituteInPlace $spicetifyDir/config-xpui.ini \
        --replace "PREFS_PATH" "$out/prefs" \
        --replace "SPOTIFY_PATH" "$out/share/spotify"

      spicetify-cli backup apply
    '';
  });
in

with lib;

{
  options = {
    programs.spicetify = {
      enable = mkEnableOption "Whether to enable Spicetify, a customized Spotify client";

      addons = mkOption {
        type = types.listOf types.package;
        default = [ ];
        description = ''
          List of derivations providing extensions, themes, and apps.
        '';
      };

      currentTheme = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = ''
          The name of the theme to use, provided by one of the addons.
        '';
      };

      colorScheme = mkOption {
        type = types.str;
        default = "base";
        description = ''
          The name of the color scheme to use.
        '';
      };

      extensions = mkOption {
        type = types.listOf types.str;
        default = [ ];
        description = ''
          List of Javascript files provided by the addons that should be loaded.
        '';
      };

      # TODO: custom_apps
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ spotify ];
  };
}
