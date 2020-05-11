{ pkgs, lib, ... }:

let
  home-manager = builtins.fetchGit {
    url = "https://github.com/rycee/home-manager.git";
    rev = "3461ceebc01169f99362ab5cc62d26224e7886d9";
    ref = "release-20.03";
  } + "/nixos";
  utils = import ../utils { inherit lib; };
in

{
  imports =
    let
      homeProgramConfigs =
        builtins.filter builtins.pathExists
        (builtins.map (d: d + "/default.nix")
        (utils.getDirs ./.));
    in [ home-manager ] ++ homeProgramConfigs;

  home-manager.users.nprin = {
    nixpkgs.config = {
      allowUnfree = true;
    };

    home = {
      # User-specific packages. If a program needs configuration, then either:
      # - Enable and configure it via home-manager, if the option is available
      #   and configuration can be done entirely in home-manager
      # - Write the configuration file and use home-manager to manage it
      packages = with pkgs; [
        # Basic CLI tools
        jq
        cloc
        gist
        gitAndTools.hub
        shellcheck
        woof

        # System monitoring
        gotop
        filelight
        neofetch

        # CS 2110
        cs2110.CircuitSim
        cs2110.cs2110docker

        # Documents
        libreoffice

        # Browsers
        firefox
        chromium
        qutebrowser

        # Jokes
        sl
        gti

        # Applications
        discord
        slack
      ];
    };
  };

}
