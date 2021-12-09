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
    rev = "ac4bed3c10eb1b045ba019c69d4ce7a6d8213abb";
    sha256 = "18hwf072vd1kc9cpcx8hnw8lk9qvf8gm6ajgr2vq9klz14d13sg1";
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

