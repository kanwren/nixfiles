{
  flake.modules = {
    darwin.shell = {
      programs.zsh.enable = false;
    };
  };
}
