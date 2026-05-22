{
  flake.modules.nixos.graphics = {
    services.libinput = {
      enable = true;
      touchpad.naturalScrolling = true;
      mouse = {
        accelProfile = "adaptive";
        accelSpeed = "1.0";
      };
    };
  };

  flake.modules.darwin.graphics = {
    system.defaults = {
      NSGlobalDomain = {
        "com.apple.swipescrolldirection" = false;
      };

      ".GlobalPreferences" = {
        "com.apple.mouse.scaling" = 3.0;
      };
    };
  };
}
