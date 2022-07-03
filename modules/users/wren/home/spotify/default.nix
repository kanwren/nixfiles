# See https://github.com/spicetify/spicetify-cli/issues/1453

{ pkgs, self, system, ... }:

let
  catppuccin = self.packages.${system}.catppuccin-spicetify;

  config = {
    Setting = {
      check_spicetify_upgrade = false;
      color_scheme = "base";
      current_theme = "catppuccin";
      inject_css = true;
      overwrite_assets = false;
      prefs_path = "PREFS_PATH";
      replace_colors = true;
      spotify_launch_flags = [ ];
      spotify_path = "SPOTIFY_PATH";
    };

    AdditionalOptions = {
      custom_apps = [ ];
      experimental_features = false;
      extensions = [ "catppuccin.js" ];
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

  spotify = pkgs.spotify.override (old: {
    spotify-unwrapped =
      let
        mkSpicetifyConfigFile =
          let
            generateConfigFile = (pkgs.formats.ini { }).generate "config-xpui.ini";
            formatConfigValues = _: v:
              if builtins.isBool v then
                if v then "1" else "0"
              else if builtins.isList v then
                builtins.concatStringsSep "|" v
              else if builtins.isAttrs v then
                builtins.mapAttrs formatConfigValues v
              else
                v;
          in
          conf: generateConfigFile (builtins.mapAttrs formatConfigValues conf);
      in
      old.spotify-unwrapped.overrideAttrs (oldAttrs: {
        nativeBuildInputs = (oldAttrs.nativeBuildInputs or [ ]) ++ [ pkgs.spicetify-cli ];
        postInstall = (oldAttrs.postInstall or "") + ''
          export HOME=$TMP
          spicetifyDir=$(dirname "$(spicetify-cli -c)")
          ln -s ${catppuccin}/catppuccin.js $spicetifyDir/Extensions/catppuccin.js
          ln -s ${catppuccin} $spicetifyDir/Themes/catppuccin
          touch $out/prefs
          cp -f ${mkSpicetifyConfigFile config} $spicetifyDir/config-xpui.ini
          substituteInPlace $spicetifyDir/config-xpui.ini \
            --replace "PREFS_PATH" "$out/prefs" \
            --replace "SPOTIFY_PATH" "$out/share/spotify"
          spicetify-cli backup apply
        '';
      });
  });
in

{
  home.packages = [ spotify ];
}

