{
  config,
  lib,
  ...
}: let
  cfg = config.mixins.haskell;
in {
  options.mixins.haskell.enable = lib.mkOption {
    type = lib.types.bool;
    default = config.mixins.enable;
    description = "Whether to enable the haskell mixin";
  };

  config = lib.mkIf cfg.enable {
    home.sessionPath = [
      "$HOME/.cabal/bin"
    ];

    home.file = {
      ".ghc/ghci.conf".source = ./ghci.conf;
      ".haskeline".source = ./haskeline.conf;
    };
  };
}
