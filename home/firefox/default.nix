{ pkgs, ... }:

{
  home-manager.users.nprin = {
    programs.firefox = {

      enable = true;

      extensions = with pkgs.nur.repos.rycee.firefox-addons; [
        vim-vixen
        darkreader
        octotree
      ];

      profiles = {
        nprin = {
          name = "nprin";
          id = 0;
          isDefault = true;
        };
      };

    };
  };
}
