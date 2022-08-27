{ pkgs, ... }:

{
  services.yabai = {
    enable = true;
    enableScriptingAddition = false;
    package = pkgs.yabai;

    config = {
      layout = "bsp";
      split_ratio = 0.50;
      window_placement = "second_child";
      window_gap = 10;
      mouse_modifier = "fn";
      mouse_action1 = "move";
      mouse_action2 = "resize";
      mouse_drop_action = "swap";
    };

    extraConfig =
      let
        don'tManage = [
          "System Preferences"
          "System Information"
          "Calculator"
          "Installer"
        ];
      in
      ''
        ${
          # TODO: may or may not want to add regex anchors
          builtins.concatStringsSep "\n" (builtins.map (x: ''
            yabai -m rule --add app="${x}" manage=off
          '') don'tManage)
        }
      '';
  };
}
