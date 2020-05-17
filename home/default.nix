{ pkgs, lib, ... }:

let
  fetchGithubArchive = { owner, repo, rev, sha256 }: fetchTarball {
    url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
    inherit sha256;
  };
  home-manager = fetchGithubArchive {
    owner = "rycee";
    repo = "home-manager";
    rev = "1ec45b11abdfbd92d608a6536d11e80bd648ec02";
    sha256 = "15p4gwkm1cz06gfd0w5g36jlnn3bpx36v0m264zqmbkhd965j3v2";
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

        # CS 2110
        cs2110.CircuitSim
        cs2110.cs2110docker

        # Documents
        libreoffice

        # Browsers
        firefox
        qutebrowser

        # Applications
        discord
        slack
      ];
    };
  };

}
