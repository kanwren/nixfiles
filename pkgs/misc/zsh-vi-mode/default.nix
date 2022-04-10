{ stdenv
, fetchFromGitHub
, lib
}:

stdenv.mkDerivation rec {
  name = "zsh-vi-mode";
  version = "unstable_2022-04-10";

  src = fetchFromGitHub {
    owner = "jeffreytse";
    repo = "zsh-vi-mode";
    rev = "9e909d0bdd0aff36ca60ea213ec92f46a554b288";
    sha256 = "1rnzgwygmy7a771n1pgw8hryrnxkagbfc9ima824g18cf5m0ks02";
  };

  installPhase = ''
    mkdir -p "$out"/share/zsh/plugins/zsh-vi-mode
    cp * "$out"/share/zsh/plugins/zsh-vi-mode
  '';

  meta = with lib; {
    description = "A better and friendly vi(vim) mode plugin for ZSH";
    homepage = "https://github.com/jeffreytse/zsh-vi-mode";
    platforms = platforms.unix;
    license = licenses.mit;
  };
}

