{ ghcVersion ? "8107", writers, haskell, ... }@args:

{ name, contents, libraries ? (_: [ ]) }:

let
  source =
    if builtins.isString contents
    then contents
    else builtins.readFile contents;
  packages = haskell.packages."ghc${ghcVersion}";
  haskellArgs = {
    ghc = packages.ghc;
    ghcArgs = [ "-O2" "-Wall" "-Werror" ];
    libraries = libraries packages;
  } // builtins.removeAttrs args [ "ghcVersion" "writers" "haskell" ];
in
writers.writeHaskellBin name haskellArgs source

