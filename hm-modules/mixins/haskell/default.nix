{
  home.sessionPath = [
    "$HOME/.cabal/bin"
  ];

  home.file = {
    ".ghc/ghci.conf".source = ./ghci.conf;
    ".haskeline".source = ./haskeline.conf;
  };
}

