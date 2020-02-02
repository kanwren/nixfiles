{ ... }:

{
  nix = {
    # cache.nixos.org is included by default
    binaryCaches = [
      # personal cache
      "https://nprindle.cachix.org"
      # haskell-ide-engine cache
      "https://all-hies.cachix.org"
    ];

    binaryCachePublicKeys = [
      "nprindle.cachix.org-1:hRW0f/n4hCZZzTzYJO9olDjJ+8MB4VpknEGpiVCxpWo="
      "all-hies.cachix.org-1:JjrzAOEUsD9ZMt8fdFbzo3jNAyEWlPAwdVuHw4RD43k="
    ];
  };
}
