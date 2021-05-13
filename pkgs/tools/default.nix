{ pkgs }:

{
  carbon-now = pkgs.callPackage ./carbon-now {
    nodejs = pkgs.nodejs-14_x;
  };
}

