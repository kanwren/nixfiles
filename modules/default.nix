{
  # a base system
  base = import ./base;

  # enable tailscale
  tailscale = import ./tailscale;

  # different components for a desktop with graphics
  desktop = {
    base = import ./desktop/base;
    x = import ./desktop/x;
    audio = import ./desktop/audio;
    bluetooth = import ./desktop/bluetooth;
    virtualisation = import ./desktop/virtualisation;
  };

  # base configs and home-manager configs for each user
  users = import ./users;

  # misc
  home-manager-base = import ./home-manager-base;
  duckdns = import ./duckdns.nix;
}
