{ stdenv
, lib
, ghcWithPackages
}:

let
  ghc = ghcWithPackages (p: with p; [
    directory filepath containers bytestring cryptonite
  ]);
  executableName = "lorri-gc";
in stdenv.mkDerivation {
  name = "lorri-gc";
  src = lib.cleanSource ./.;
  buildPhase = "${ghc}/bin/ghc -O2 Main.hs -o ${executableName}";
  installPhase = ''
    mkdir -p "$out/bin"
    mv ${executableName} "$out/bin"
  '';
}
