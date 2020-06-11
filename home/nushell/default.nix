{ pkgs, ... }:

{
  home-manager.users.nprin = {

    imports = [ ./nushell.nix ];

    programs.nushell = {
      enable = true;
      # nu fails to start when config.toml is readonly, so managing this with HM
      # is blocked by nushell/#1963
      # settings = {
      #   edit_mode = "vi";
      #   key_timeout = "0";
      #   pivot_mode = "never";
      # };
    };

  };
}

