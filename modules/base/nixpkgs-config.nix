let
  module =
    { config, lib, ... }:
    {
      options.nixpkgs = {
        allowedBrokenPackages = lib.mkOption {
          type = with lib.types; nullOr (listOf str);
          default = null;
          example = [ ];
        };

        allowedUnfreePackages = lib.mkOption {
          type = with lib.types; nullOr (listOf str);
          default = null;
          example = [ "nvidia-x11" ];
        };
      };

      config =
        lib.mkIf
          (config.nixpkgs.allowedUnfreePackages != null || config.nixpkgs.allowedUnfreePackages != null)
          {
            nixpkgs.config =
              lib.optionalAttrs (config.nixpkgs.allowedUnfreePackages != null) {
                allowedUnfreePredicate = p: builtins.elem (lib.getName p) config.nixpkgs.allowedUnfreePackages;
              }
              // lib.optionalAttrs (config.nixpkgs.allowedBrokenPackages != null) {
                allowedBrokenPredicate = p: builtins.elem (lib.getName p) config.nixpkgs.allowedBrokenPackages;
              };
          };
    };
in
{
  flake.modules = {
    nixos.base = module;
    darwin.base = module;
  };
}
