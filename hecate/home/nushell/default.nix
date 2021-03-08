{ pkgs, ... }:

{
  programs.nushell = {
    enable = true;
    settings = {
      key_timeout = 0;
      pivot_mode = "never";
      line_editor = {
        edit_mode = "vi";
      };
      prompt = "echo $(${pkgs.starship}/bin/starship prompt)";
      startup = [];
    };
  };
}

