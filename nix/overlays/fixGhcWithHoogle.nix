# Patch ghcWithHoogle to filter out null packages, where it would otherwise
# break when given any of the boot packages

self: super:

let
  inherit (super) lib;

  ghcOverride = input: ovl: input.override (old: {
    overrides = lib.composeExtensions (old.overrides or (_: _: { })) ovl;
  });

  fixGhcWithHoogle = input: ghcOverride input (hself: hsuper: {
    # Compose the selector with a null filter to fix error on null packages
    ghcWithHoogle = selector:
      hsuper.ghcWithHoogle (ps: builtins.filter (x: x != null) (selector ps));
    ghc = hsuper.ghc // { withHoogle = hself.ghcWithHoogle; };
  });
in {
  haskell = super.haskell // {
    packages = super.haskell.packages // {
      ghc883 = fixGhcWithHoogle super.haskell.packages.ghc883;
      ghc8101 = fixGhcWithHoogle super.haskell.packages.ghc8101;
    };
  };
}

