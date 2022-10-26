{ self }:

{
  mixins = import ./mixins { inherit self; };

  duckdns = import ./duckdns.nix;
}
