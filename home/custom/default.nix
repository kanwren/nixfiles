# Custom-built user packages

{ pkgs, ... }:

let
  customPackages = {

    githug = pkgs.buildRubyGem {
      inherit (pkgs) ruby;
      pname = "githug";
      version = "0.5.0";
      gemName = "githug";
      source.sha256 = "0hk4pvbvjxipapzjr9rrhcvm2mxlw4a8f6bsfqgq1wnvlbmmrzc6";
    };

  };
in {
  home-manager.users.nprin = {
    home.packages = builtins.attrValues customPackages;
  };
}

