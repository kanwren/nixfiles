{ ... }:

{
  nix = {
    # cache.nixos.org is included by default
    binaryCaches = [
      "https://nprindle.cachix.org"
      "http://cache.earthtools.ca"
      "https://hydra.iohk.io"
      # haskell-ide-engine cache
      "https://all-hies.cachix.org"
    ];

    binaryCachePublicKeys = [
      "nprindle.cachix.org-1:hRW0f/n4hCZZzTzYJO9olDjJ+8MB4VpknEGpiVCxpWo="
      "c2d.localnet-1:YTVKcy9ZO3tqPNxRqeYEYxSpUH5C8ykZ9ImUKuugf4c="
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "all-hies.cachix.org-1:JjrzAOEUsD9ZMt8fdFbzo3jNAyEWlPAwdVuHw4RD43k="
    ];
  };
}
