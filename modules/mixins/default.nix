{ self }:

{
  # a base system
  base = import ./base { inherit self; };

  # enable tailscale
  tailscale = import ./tailscale;

  # different components for a desktop with graphics
  desktop = import ./desktop;

  home-manager-common = import ./home-manager-common;
}

