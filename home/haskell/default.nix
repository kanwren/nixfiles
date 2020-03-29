{ pkgs, ... }:

{
  home-manager.users.nprin = {

    home.packages = with pkgs; [
      cabal-install
      ghcid
      haskellPackages.hasktags
      (all-hies.selection { selector = p: { inherit (p) ghc865 ghc881; }; })
      # From summoner overlay
      haskellPackages.summoner
      haskellPackages.summoner-tui
    ];

    home.file = {
      ".summoner.toml".source = ./summoner.toml;
    };

  };
}


