self:

{
  mixins = {
    # a base system
    base = import ./mixins/base;

    # enable tailscale
    tailscale = import ./mixins/tailscale;

    # different components for a desktop with graphics
    desktop = import ./mixins/desktop;

    # base configs and home-manager configs for each user
    users = import ./mixins/users self;

    home-manager = import ./mixins/home-manager;
  };

  duckdns = import ./duckdns.nix;
}
