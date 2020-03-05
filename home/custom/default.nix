# Custom-built user packages

{ pkgs, ... }:

let
  customPackages = {

    infinisweep = import (pkgs.fetchFromGitHub {
      owner = "basile-henry";
      repo = "infinisweep";
      rev = "5969f2ff96dbfe3212f342ca0818ea2908a631ac";
      sha256 = "1lqvkfsz96df8i7628mjkm1az9rbmb5mibb2xlgxaf5px7lmr2qf";
    }) {};

    githug = pkgs.buildRubyGem {
      inherit (pkgs) ruby;
      pname = "githug";
      version = "0.5.0";
      gemName = "githug";
      source.sha256 = "0hk4pvbvjxipapzjr9rrhcvm2mxlw4a8f6bsfqgq1wnvlbmmrzc6";
    };

    py2048 = import ./2048.nix { inherit pkgs; };

  };
in {
  home-manager.users.nprin = {
    home.packages = builtins.attrValues customPackages;
  };
}

