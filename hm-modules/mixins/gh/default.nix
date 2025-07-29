{ pkgs
, config
, lib
, ...
}:
let
  cfg = config.mixins.gh;
in
{
  options.mixins.gh.enable = lib.mkOption {
    type = lib.types.bool;
    default = config.mixins.enable;
    description = "Whether to enable the gh mixin";
  };

  config = lib.mkIf cfg.enable {
    programs.gh = {
      enable = true;

      extensions = [
        pkgs.gh-copilot
        pkgs.gh-ost
      ];

      settings = {
        git_protocol = "https";
        prompt = "enabled";
        aliases = {
          co = "pr checkout";
        };
      };
    };
  };
}
