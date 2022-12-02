{
  nix.settings = {
    # cache.nixos.org is included by default
    substituters = [
      "https://nprindle.cachix.org"
      # "http://cache.earthtools.ca"
      # haskell-ide-engine cache
      "https://all-hies.cachix.org"
    ];

    trusted-public-keys = [
      "nprindle.cachix.org-1:hRW0f/n4hCZZzTzYJO9olDjJ+8MB4VpknEGpiVCxpWo="
      # "c2d.localnet-1:YTVKcy9ZO3tqPNxRqeYEYxSpUH5C8ykZ9ImUKuugf4c="
      "all-hies.cachix.org-1:JjrzAOEUsD9ZMt8fdFbzo3jNAyEWlPAwdVuHw4RD43k="
    ];
  };
}
