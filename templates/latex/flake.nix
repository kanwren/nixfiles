{
  description = "A basic LaTeX project";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixpkgs-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      defaultSystems = [ "aarch64-darwin" "aarch64-linux" "x86_64-darwin" "x86_64-linux" ];
      forAllSystems = f:
        nixpkgs.lib.genAttrs
          defaultSystems
          (system:
            let
              pkgs = nixpkgs.legacyPackages.${system};
              appliedOverlay = self.overlays.default pkgs pkgs;
            in
            f (pkgs // appliedOverlay));
    in
    {
      overlays.default = final: prev:
        final.callPackages ./default.nix { };

      packages = forAllSystems (pkgs: {
        default = pkgs.docs.main;
      });

      devShells = forAllSystems (pkgs: {
        default = pkgs.callPackage ./shell.nix { };
      });
    };
}
