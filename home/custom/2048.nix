{ pkgs }:

with pkgs;

stdenv.mkDerivation {
  name = "2048";
  src = fetchFromGitHub {
    owner = "csheldonhess";
    repo = "2048";
    rev = "50cbd8368f1a4922da007e80b3dd71899c2f73fc";
    sha256 = "07brlkdvz44kwn2ni0df11wxsg459w92nbannd5fcwgdg86x76ys";
  };
  buildInputs = [ makeWrapper ];
  propagatedBuildInputs = [ python3 ];
  buildPhase = "true";
  installPhase = ''
    mkdir -p "$out"
    cp 2048.py "$out"
    makeWrapper "${python3}/bin/python3" "$out/bin/2048" --add-flags "$out/2048.py"
  '';
}

