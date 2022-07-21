self:

{
  mixins = {
    # a base system
    base = import ./mixins/base;

    # enable tailscale
    tailscale = import ./mixins/tailscale;

    # different components for a desktop with graphics
    desktop = {
      base = import ./mixins/desktop/base;
      x = import ./mixins/desktop/x;
      audio = import ./mixins/desktop/audio;
      bluetooth = import ./mixins/desktop/bluetooth;
      virtualisation = import ./mixins/desktop/virtualisation;
    };

    # base configs and home-manager configs for each user
    users = import ./mixins/users self;

    home-manager = import ./mixins/home-manager;
  };

  duckdns = import ./duckdns.nix;
}
