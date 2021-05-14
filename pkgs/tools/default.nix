{ pkgs }:

{
  carbon-now-cli = pkgs.callPackage ./carbon-now-cli {
    nodejs = pkgs.nodejs-14_x;
  };
}

