{ stdenv
, texlive
,
}: rec {
  tex-env = texlive.combine {
    inherit
      (texlive)
      latexmk
      # Any extra libraries here
      # enumitem
      scheme-small
      ;
  };

  docs.main = stdenv.mkDerivation {
    name = "main";
    src = ./.;
    buildInputs = [ tex-env ];
    buildPhase = "make clean && HOME=. make";
  };
}
