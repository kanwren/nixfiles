{ pkgs, ... }:

{
  home-manager.users.nprin = {
    programs.firefox = {

      enable = true;

      extensions = with pkgs.nur.repos.rycee.firefox-addons; [
        vim-vixen
        darkreader
      ];

      profiles = {
        nprin = {
          name = "nprin";
          id = 0;
          isDefault = true;
          settings = {
            "browser.contentblocking.category" = "standard";
            "browser.tabs.warnOnClose" = false;
            "extensions.activeThemeID" = "firefox-compact-dark@mozilla.org";
          };
          userChrome = builtins.readFile ./userChrome.css;
        };
      };

    };
  };
}
