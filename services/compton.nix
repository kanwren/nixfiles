{ config, pkgs, ... }:

{
  services.compton = {
    enable = true;

    # activeOpacity = "0.85";
    # inactiveOpacity = "0.9";
    opacityRules = [
      "85:class_g = 'Alacritty'"
    ];

    shadow = true;

    # 4ms fade when changing opacity or opening windows
    fade = true;
    fadeDelta = 4;
  };
}
