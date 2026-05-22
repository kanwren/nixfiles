{
  flake.modules.darwin."users/wrenn" = {
    homebrew.casks = [ "kitty" ];
  };

  flake.modules.homeManager."users/wrenn" =
    { pkgs, ... }:
    {
      programs.kitty = {
        enable = true;
        package = pkgs.kitty.overrideAttrs { doInstallCheck = false; };
        extraConfig = builtins.readFile ./kitty.conf;
        shellIntegration.mode = "enabled";
      };
    };
}
