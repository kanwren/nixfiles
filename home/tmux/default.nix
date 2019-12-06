{ config, pkgs, ... }:

{
  home-manager.users.nprin = {

    # TODO: enable tmux and configure plugins

    home.packages = [ pkgs.tmux ];

    home.file = {
      ".tmux.conf".source = ./tmux.conf;
    };

  };
}

