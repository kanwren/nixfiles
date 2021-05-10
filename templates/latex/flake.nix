{
  description = "A basic LaTeX project";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, flake-compat }:
    flake-utils.lib.eachDefaultSystem (system: (
      let
        pkgs = nixpkgs.legacyPackages.${system};
        docname = "main";
        tex-env = with pkgs; texlive.combine {
          inherit (texlive)
            latexmk
            # Any extra libraries here
            # enumitem
            scheme-small;
        };
      in
      {
        defaultPackage = self.packages.${system}.${docname};

        packages.${docname} = pkgs.stdenv.mkDerivation {
          name = docname;
          src = ./.;
          buildInputs = [ tex-env ];
          buildPhase = "make clean && HOME=. make";
        };

        devShell = pkgs.mkShell {
          buildInputs = [ tex-env ];
        };
      }
    ));
}
