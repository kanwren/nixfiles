{
  flake.modules.nixos.graphics = {
    services.printing = {
      enable = true;
      openFirewall = true;
    };
  };
}
