{ stdenv
, fetchFromGitHub
}:

stdenv.mkDerivation {
  name = "kak-readline";
  src = fetchFromGitHub {
    owner = "chambln";
    repo = "kakoune-readline";
    rev = "8029c0eee75d41401184c06620bf0f45240d9a14";
    sha256 = "180s960g6y7gqjj20i4k4ihp6ajyf3b0bbhchqrlxvqzpaxxbrd5";
  };
  installPhase = ''
    mkdir -p "$out"/share/kak/autoload/plugins/readline
    cp -r readline.kak "$out"/share/kak/autoload/plugins/readline
  '';
}

