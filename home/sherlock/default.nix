{ pkgs, ... }:

let
  enableTor = true;

  # Python environment with overridden packages for running sherlock
  sherlockPy =
    let
      python37 = pkgs.callPackage (import ./sherlock-python.nix) {};
    in python37.withPackages (ps: with ps; [
      beautifulsoup4
      soupsieve
      certifi
      colorama
      lxml
      requests-futures
      torrequest
    ]);

  sherlock = pkgs.stdenv.mkDerivation {
    name = "sherlock";
    src = pkgs.fetchFromGitHub {
      owner = "sherlock-project";
      repo = "sherlock";
      rev = "f3a61fe7afa4abc0305c802e51c814c3d31d4b56";
      sha256 = "15h13ljhqi53qjsybj75r2ldqzifvxvnml9zy0sbgav7ilqb4v14";
    };
    buildInputs = with pkgs; [ makeWrapper ];
    propagatedBuildInputs = [ sherlockPy ] ++ pkgs.lib.optional enableTor pkgs.tor;
    buildPhase = ''
      mkdir -p "$out/lib"
      cp *.json "$out/lib"
      cp *.py "$out/lib"
    '';
    installPhase = ''
      mkdir -p "$out/bin"
      makeWrapper \
      "${sherlockPy}/bin/python3" \
      "$out/bin/sherlock" \
      --argv0 "sherlock" \
      ${pkgs.lib.optionalString enableTor "--prefix PATH \":\" \"${pkgs.tor}/bin\""} \
      --add-flags "$out/lib/sherlock.py"
    '';
  };

in {
  home-manager.users.nprin = {
    home.packages = [ sherlock ];
  };
}
