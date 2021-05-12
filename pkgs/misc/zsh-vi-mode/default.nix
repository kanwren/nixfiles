{ stdenv
, fetchFromGitHub
, lib
}:

stdenv.mkDerivation rec {
  name = "zsh-vi-mode";
  version = "0.8.3";

  src = fetchFromGitHub {
    owner = "jeffreytse";
    repo = "zsh-vi-mode";
    rev = "v${version}";
    sha256 = "sha256-xpwRk7Om286kFpWf3c621zDoghxWYHFbeFUk1ReaTI0=";
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

