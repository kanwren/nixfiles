{ stdenv
, fetchFromGitHub
, lib
}:

stdenv.mkDerivation {
  name = "kak-split-object";
  src = fetchFromGitHub {
    owner = "alexherbo2";
    repo = "split-object.kak";
    rev = "1c52ab3fe5ed10042c999cf99c34b8582e037d9f";
    sha256 = "14z9jh5lkgzgms703wwcghldpd0d588w0xys5gv47xw9jbmjlpsx";
  };
  installPhase = ''
    mkdir -p "$out"/share/kak/autoload/plugins/split-object
    cp -r rc/*.kak "$out"/share/kak/autoload/plugins/split-object
  '';
  meta = with lib; {
    description = "Split object for Kakoune";
    homepage = "https://github.com/alexherbo2/split-object.kak";
  };
}

