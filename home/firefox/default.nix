{ config, pkgs, ... }:

{
  home-manager.users.nprin = {
    programs.firefox = {

      enable = true;

      extensions = with pkgs.nur.repos.rycee.firefox-addons; [
        vim-vixen
        darkreader
      ];

    };
  };
}
