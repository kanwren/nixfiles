{
  flake.modules.homeManager.shell = {
    programs.direnv = {
      enable = true;
      nix-direnv.enable = true;
    };
  };
}
