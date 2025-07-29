{ slides
, graph-easy
, plantuml
, perlPackages
, fetchurl
, lib
, symlinkJoin
,
}:
let
  graph_easy_as_svg = perlPackages.buildPerlPackage {
    pname = "Graph-Easy-As_svg";
    version = "0.28";
    src = fetchurl {
      url = "mirror://cpan/authors/id/S/SH/SHLOMIF/Graph-Easy-As_svg-0.28.tar.gz";
      sha256 = "sha256-zZpfrERELzSzN5CB6GwvcUAE5TkkYAbJgUERbL8+J3g=";
    };
    doCheck = false;

    meta = with lib; {
      description = "Output a Graph::Easy as Scalable Vector Graphics (SVG)";
      license = licenses.gpl1Only;
      platforms = platforms.unix;
    };
  };

  graph-easy-full = graph-easy.overrideAttrs (old: {
    propagatedBuildInputs = (old.propagatedBuildInputs or [ ]) ++ [ graph_easy_as_svg ];
  });
in
symlinkJoin {
  name = "slides-full";
  paths = [
    slides
    graph-easy-full
    plantuml
  ];
}
