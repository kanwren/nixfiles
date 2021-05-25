{ stdenv
, fetchFromGitHub
, lib
}:

stdenv.mkDerivation rec {
  name = "zsh-vi-mode";
  version = "unstable_2021-05-20";

  src = fetchFromGitHub {
    owner = "jeffreytse";
    repo = "zsh-vi-mode";
    rev = "3b3f5f902f8e60ace5ef1a1f089d9580549fc1de";
    sha256 = "sha256-9Vw/yVhLW216el5046n17b4jsTZGoBmeBa0+5vDEtdM=";
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

