{
  time = {
    timeZone = "America/Los_Angeles";
    hardwareClockInLocalTime = true;
  };

  # redshift is broken with geoclue2; use manual location provider in the meantime: https://github.com/jonls/redshift/issues/895
  location = {
    provider = "manual";
    latitude = 34.052235;
    longitude = -118.243683;
  };

  # TODO: temporarily broken
  services.localtimed.enable = false;
}
