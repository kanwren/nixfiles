{ ... }:

{
  services.picom = {
    enable = true;

    # activeOpacity = "0.85";
    # inactiveOpacity = "0.9";
    opacityRules = [
      # Names can be determined using xprop
      "85:class_g = 'kitty'"
    ];

    shadow = true;

    # 4ms fade when changing opacity or opening windows
    fade = true;
    fadeDelta = 4;
  };
}
