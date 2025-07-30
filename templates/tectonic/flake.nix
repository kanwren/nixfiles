{
  description = "A basic LaTeX project using tectonic";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixpkgs-unstable";
  };

  outputs = {
    self,
    nixpkgs,
  }: let
    defaultSystems = ["aarch64-darwin" "aarch64-linux" "x86_64-darwin" "x86_64-linux"];
    forAllSystems = f:
      nixpkgs.lib.genAttrs
      defaultSystems
      (system: let
        pkgs = nixpkgs.legacyPackages.${system};
      in
        f pkgs);
  in {
    devShells = forAllSystems (pkgs: {
      default = pkgs.mkShell {
        packages = with pkgs; [tectonic];
      };
    });
  };
}
