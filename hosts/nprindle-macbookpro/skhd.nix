{ pkgs, ... }:

{
  services.skhd = {
    # https://github.com/foreverd34d/.dotfiles/blob/master/skhd/skhdrc
    enable = true;
    # note: not all of this works without disabling SIP
    skhdConfig = with pkgs; ''
      ctrl + alt - t : yabai -m config layout bsp
      ctrl + alt - s : yabai -m config layout stack
      ctrl + alt - f : yabai -m config layout float

      alt - h : yabai -m window --focus west
      alt - j : yabai -m window --focus south
      alt - k : yabai -m window --focus north
      alt - l : yabai -m window --focus east

      alt - n : yabai -m query --spaces --space | ${jq}/bin/jq -re ".index" | xargs -I{} yabai -m query --windows --space {} | ${jq}/bin/jq -sre "add | map(select(.minimized != 1)) | sort_by(.display, .frame.y, .frame.x, .id) | reverse | nth(index(map(select(.focused == 1))) - 1).id" | xargs -I{} yabai -m window --focus {}
      alt - p : yabai -m query --spaces --space | ${jq}/bin/jq -re ".index" | xargs -I{} yabai -m query --windows --space {} | ${jq}/bin/jq -sre "add | map(select(.minimized != 1)) | sort_by(.display, .frame.y, .frame.y, .id) | nth(index(map(select(.focused == 1))) - 1).id" | xargs -I{} yabai -m window --focus {}
      # alt - n : yabai -m window --focus stack.next
      # alt - p : yabai -m window --focus stack.prev

      shift + alt - h : yabai -m window --swap west
      shift + alt - j : yabai -m window --swap south
      shift + alt - k : yabai -m window --swap north
      shift + alt - l : yabai -m window --swap east

      # < / >
      alt - 0x2B : yabai -m space --focus prev
      alt - 0x2F : yabai -m space --focus next
      alt - 1 : yabai -m space --focus 1
      alt - 2 : yabai -m space --focus 2
      alt - 3 : yabai -m space --focus 3
      alt - 4 : yabai -m space --focus 4
      alt - 5 : yabai -m space --focus 5
      alt - 6 : yabai -m space --focus 6
      alt - 7 : yabai -m space --focus 7
      alt - 8 : yabai -m space --focus 8

      shift + alt - 0x2B : yabai -m window --space prev
      shift + alt - 0x2F : yabai -m window --space next
      shift + alt - 1 : yabai -m window --space 1
      shift + alt - 2 : yabai -m window --space 2
      shift + alt - 3 : yabai -m window --space 3
      shift + alt - 4 : yabai -m window --space 4
      shift + alt - 5 : yabai -m window --space 5
      shift + alt - 6 : yabai -m window --space 6
      shift + alt - 7 : yabai -m window --space 7
      shift + alt - 8 : yabai -m window --space 8

      ctrl + shift + alt - 0x2B : yabai -m window --space prev; yabai -m space --focus prev
      ctrl + shift + alt - 0x2F : yabai -m window --space next; yabai -m space --focus next
      ctrl + shift + alt - 1 : yabai -m window --space 1; yabai -m space --focus 1
      ctrl + shift + alt - 2 : yabai -m window --space 2; yabai -m space --focus 2
      ctrl + shift + alt - 3 : yabai -m window --space 3; yabai -m space --focus 3
      ctrl + shift + alt - 4 : yabai -m window --space 4; yabai -m space --focus 4
      ctrl + shift + alt - 5 : yabai -m window --space 5; yabai -m space --focus 5
      ctrl + shift + alt - 6 : yabai -m window --space 6; yabai -m space --focus 6
      ctrl + shift + alt - 7 : yabai -m window --space 7; yabai -m space --focus 7
      ctrl + shift + alt - 8 : yabai -m window --space 8; yabai -m space --focus 8

      # TODO: displays!

      ctrl + alt - n : yabai -m space --create && yabai -m space --focus next
      ctrl + alt - x : yabai -m space --destroy
      ctrl + alt - g : yabai -m space --toggle padding; yabai -m space --toggle gap
      ctrl + alt - b : yabai -m space --balance
      ctrl + alt - q : yabai -m window --close
      alt - r : yabai -m space --rotate 90
      alt - y : yabai -m space --mirror y-axis
      alt - u : yabai -m space --mirror x-axis
      ctrl + alt - space : yabai -m window --toggle float
      alt - f : yabai -m window --toggle zoom-fullscreen
      shift + alt - f : yabai -m window --toggle native-fullscreen
    '';
  };
}

