{ stdenv
, fetchFromGitHub
, lib
}:

stdenv.mkDerivation {
  name = "kak-readline";
  src = fetchFromGitHub {
    owner = "chambln";
    repo = "kakoune-readline";
    rev = "8029c0eee75d41401184c06620bf0f45240d9a14";
    sha256 = "sha256-peXVu7of704zhgyuBdZwXipzYSSTRCCkxO9484BJGqA=";
  };
  installPhase = ''
    mkdir -p "$out"/share/kak/autoload/plugins/readline
    cp -r readline.kak "$out"/share/kak/autoload/plugins/readline
  '';
  meta = with lib; {
    description = "Readline-style mappings for Kakoune";
    homepage = "https://github.com/chambln/kakoune-readline";
    licenses = licenses.mit;
  };
}

