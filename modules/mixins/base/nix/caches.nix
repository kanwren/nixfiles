{
  nix.settings = {
    # cache.nixos.org is included by default
    substituters = [
      # haskell-ide-engine cache
      "https://all-hies.cachix.org"
    ];

    trusted-public-keys = [
      "all-hies.cachix.org-1:JjrzAOEUsD9ZMt8fdFbzo3jNAyEWlPAwdVuHw4RD43k="
    ];
  };
}
