{
  flake.modules.homeManager.shell = {
    programs.yazi = {
      enable = true;
      shellWrapperName = "y";
      enableBashIntegration = true;
      enableZshIntegration = true;
      enableFishIntegration = true;
    };
  };
}
