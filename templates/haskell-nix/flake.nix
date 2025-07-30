{
  description = "haskell.nix flake template";
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = {
    self,
    nixpkgs,
    flake-utils,
    haskellNix,
  }:
    flake-utils.lib.eachSystem ["x86_64-linux" "x86_64-darwin"] (system: let
      overlays = [
        haskellNix.overlay
        (final: prev: {
          quux-project = final.haskell-nix.project' {
            src = ./.;
            compiler-nix-name = "ghc8107";
            shell = {
              tools = {
                cabal = {};
                hlint = {};
                haskell-language-server = {};
              };
              buildInputs = with pkgs; [
                nixpkgs-fmt
              ];
              withHoogle = true;
            };
          };
        })
      ];
      pkgs = import nixpkgs {
        inherit system overlays;
        inherit (haskellNix) config;
      };
      flake = pkgs.quux-project.flake {};
    in
      flake
      // {
        packages.default = flake.packages."quux:exe:quux";
      });
}
