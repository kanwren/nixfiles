{ pkgs, ... }:

{
  home-manager.users.nprin = {

    imports = [ ./nushell.nix ];

    programs.nushell = {
      enable = true;
      settings = {
        edit_mode = "vi";
        key_timeout = 0;
        pivot_mode = "never";
      };
    };

  };
}

