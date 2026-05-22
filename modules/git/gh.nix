{
  flake.modules.homeManager.git =
    { pkgs, ... }:
    {
      home.packages = [ pkgs.gh ];
    };
}
