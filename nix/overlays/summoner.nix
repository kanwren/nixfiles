# Fix broken summoner build
self: super:

let
  summonerSrc = super.fetchFromGitHub {
    owner = "nprindle";
    repo = "summoner";
    rev = "8a6fccf2c95244ac5934b82f794562ce6cd765b4";
    sha256 = "0p5cgn3k033iij577rrw77cpv3lfq1ag27qykh3134dy58dbrpyw";
  };
  summonerHaskellPkgs = import summonerSrc {};
in {
  haskellPackages = super.haskellPackages.override {
    overrides = hself: hsuper: {
      inherit (summonerHaskellPkgs) summoner summoner-tui;
    };
  };
}
