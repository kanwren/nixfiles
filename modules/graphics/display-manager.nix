{
  flake.modules.nixos.graphics = {
    services.xserver = {
      enable = true;
      displayManager.lightdm = {
        enable = true;
        greeters.gtk = {
          enable = true;
          clock-format = "%I:%M %p";
        };
      };
    };
  };
}
