{
  flake.modules.homeManager.zellij =
    { lib, ... }:
    {
      programs.zellij = {
        enable = true;
        enableBashIntegration = false;
        enableZshIntegration = false;
        enableFishIntegration = false;
      };

      home.sessionVariables = {
        ZELLIJ_AUTO_ATTACH = "false";
        ZELLIJ_AUTO_EXIT = "false";
      };

      # NOTE: the home-manager toKDL generator represents node children as an
      # attrset, which is the wrong type for KDL's model (can't repeat keys, and
      # children are supposed to be ordered). This makes it impossible to use
      # toKDL to generate repeated children with the same node name, as is needed
      # here for keybindings.
      xdg.configFile."zellij/config.kdl" = lib.mkForce {
        source = ./config.kdl;
      };
    };
}
