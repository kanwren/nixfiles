{ ... }:

{
  nix = {
    # cache.nixos.org is included by default
    binaryCaches = [
      # personal cache
      "https://nprindle.cachix.org"
    ];

    binaryCachePublicKeys = [
      "nprindle.cachix.org-1:hRW0f/n4hCZZzTzYJO9olDjJ+8MB4VpknEGpiVCxpWo="
    ];
  };
}
