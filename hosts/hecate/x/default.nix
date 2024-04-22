{ ... }:

{
  imports = [
    ./display-manager.nix
    ./fonts.nix
    ./i3
  ];

  services.xserver = {
    enable = true;

    # Enable touchpad
    libinput = {
      enable = true;
      touchpad.naturalScrolling = true;
      mouse = {
        accelProfile = "adaptive";
        accelSpeed = "1.0";
      };
    };

    xkb = {
      layout = "us";
      options = "caps:escape"; # TODO: set good compose key
    };
  };
}
