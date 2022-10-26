{
  programs.zsh = {
    enable = true;
    history = {
      save = 100000;
      size = 100000;
      expireDuplicatesFirst = true;
    };
  };
}
