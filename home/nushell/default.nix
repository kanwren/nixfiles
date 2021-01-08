{ pkgs, ... }:

{
  programs.nushell = {
    # TODO: update nushell configs for much newer version
    enable = false;
    settings = {
      edit_mode = "vi";
      key_timeout = 0;
      pivot_mode = "never";
      startup = [];
    };
  };
}

