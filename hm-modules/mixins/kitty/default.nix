{ ... }:

{
  programs.kitty = {
    enable = true;
    extraConfig = builtins.readFile ./kitty.conf;
    shellIntegration.mode = "enabled";
  };
}

