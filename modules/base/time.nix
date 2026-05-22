{
  flake.modules.nixos.base = {
    time.timeZone = "America/Los_Angeles";

    location = {
      provider = "manual";
      latitude = 34.052235;
      longitude = -118.243683;
    };

    services.localtimed.enable = false;
  };
}
