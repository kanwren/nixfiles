{ config, pkgs, ... }:

{
  home-manager.users.nprin = {

    # programs.tmux = {
    #   enable = true;
    # };

    home.packages = [ pkgs.tmux ];

    home.file = {
      ".tmux.conf".source = ./tmux.conf;
    };

  };
}

