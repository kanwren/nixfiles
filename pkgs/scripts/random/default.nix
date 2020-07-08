{ stdenv
, lib
, ghcWithPackages
}:

let
  ghc = ghcWithPackages (p: with p; [ splitmix optparse-applicative ]);
  executableName = "random";
in stdenv.mkDerivation {
  name = "${executableName}";
  src = lib.cleanSource ./.;
  buildPhase = "${ghc}/bin/ghc -O2 Main.hs -o ${executableName}";
  installPhase = ''
    mkdir -p "$out/bin"
    mv ${executableName} "$out/bin"
  '';
}
