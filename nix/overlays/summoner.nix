# Fix broken summoner build
self: super:

let
  summonerSrc = super.fetchFromGitHub {
    owner = "nprindle";
    repo = "summoner";
    rev = "2d074a36b3e5d675f787c341e6927b8c0bb70e9a";
    sha256 = "1cf32qwsakk7fhgaa6bmyfdn70dwnnnnkfhff2pvajp4xpg8hkpd";
  };
  summonerHaskellPkgs = import summonerSrc {};
in {
  haskellPackages = super.haskellPackages.override {
    overrides = hself: hsuper: {
      inherit (summonerHaskellPkgs) summoner summoner-tui;
    };
  };
}
