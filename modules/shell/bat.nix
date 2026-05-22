{
  flake.modules.homeManager.shell =
    { pkgs, ... }:
    {
      programs.bat = {
        enable = true;
        extraPackages = [ pkgs.bat-extras.core ];
      };
    };
}
