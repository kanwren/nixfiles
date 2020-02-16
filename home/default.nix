{ pkgs, lib, ... }:

let
  home-manager = builtins.fetchGit {
    url = "https://github.com/rycee/home-manager.git";
    rev = "dff5f07952e61da708dc8b348ea677414e992215";
    ref = "release-19.09";
  } + "/nixos";
  utils = import ../utils { inherit lib; };
in

with rec {
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
};

{
  imports =
    let
      homeProgramConfigs =
        builtins.filter builtins.pathExists
        (builtins.map (d: d + "/default.nix")
        (utils.getDirs ./.));
    in [ home-manager ] ++ homeProgramConfigs;

  home-manager.users.nprin = {
    home = {
      # User-specific packages. If a program needs configuration, then either:
      # - Enable and configure it via home-manager, if the option is available
      #   and configuration can be done entirely in home-manager
      # - Write the configuration file and use home-manager to manage it
      packages = with pkgs; [
        # Basic CLI tools
        ag
        tldr
        jq
        cloc
        gist
        gitAndTools.hub
        shellcheck

        # File explorers
        ranger

        # System monitoring
        gotop
        filelight
        neofetch

        # Development
        openjdk11
        nodejs-12_x
        nodePackages.node2nix

        # CS 2110
        cs2110.CircuitSim
        cs2110.complx-tools
        cs2110.cs2110docker

        # Documents
        libreoffice

        # Browsers
        firefox
        chromium
        unstable.torbrowser

        # Games
        infinisweep
        githug

        # Applications
        discord
        slack
        spotify
        musescore
        #unstable.steam
        wineUnstable
      ];
    };
  };

}
