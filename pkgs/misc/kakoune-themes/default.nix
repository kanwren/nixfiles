{ stdenv
, fetchFromGitHub
}:

stdenv.mkDerivation {
  name = "kakoune-themes";
  src = fetchFromGitHub {
    owner = "anhsirk0";
    repo = "kakoune-themes";
    rev = "58e18355fd94434314da7be2db81df72a7805218";
    sha256 = "sha256-ITir5QPhY0gJAj8+YO8I1K+vTSwdbvaP39khY5CRpG0=";
  };
  installPhase = ''
    target="$out"/share/kak
    mkdir -p "$target"
    cp -r colors "$target"
  '';
}
