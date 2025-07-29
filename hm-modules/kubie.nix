{ pkgs
, lib
, config
, ...
}:
let
  cfg = config.programs.kubie;

  settingsFormat = pkgs.formats.yaml { };
in
{
  options.programs.kubie = {
    enable = lib.mkEnableOption "kubie";

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.kubie;
      description = "The kubie package to use";
    };

    settings = lib.mkOption {
      inherit (settingsFormat) type;
      default = { };
      description = "kubie settings; see https://github.com/sbstp/kubie#settings for options";
    };
  };

  config = lib.mkIf cfg.enable {
    home = {
      packages = [ cfg.package ];
      file.".kube/kubie.yaml".source = settingsFormat.generate "kubie.yaml" cfg.settings;
    };
  };
}
