{ python37
, fetchFromGitHub
, lib
}:

python37.override {
  packageOverrides = pself: psuper: {
    stem = psuper.stem.overrideAttrs (attrs: {
      src = fetchFromGitHub {
        owner = "torproject";
        repo = "stem";
        rev = "7852f528d34893de3fd6dfc87f299c52f11ffddf";
        sha256 = "17ab1isvk330bvrrzq90rj9z2k4a3m7bxnggxdav8gyrw17yd1cn";
      };
    });

    requests-futures = psuper.buildPythonPackage rec {
      pname = "requests-futures";
      version = "1.0.0";
      src = psuper.fetchPypi {
        inherit pname version;
        sha256 = "0j611g1wkn98qp2b16kqz7lfz29a153jyfm02r3h8n0rpw17am1m";
      };
      propagatedBuildInputs = with pself; [ requests ];
      doCheck = false;
      meta = with lib; {
        homepage = "https://github.com/ross/requests-futures";
        license = licenses.asl20;
      };
    };

    torrequest = psuper.buildPythonPackage rec {
      pname = "torrequest";
      version = "0.1.0";
      src = psuper.fetchPypi {
        inherit pname version;
        sha256 = "10mf9p6r4wwk9m50jh5bpmvsnymkmn3wfqs30dx8vagx7zmd8i9p";
      };
      propagatedBuildInputs = with pself; [ pysocks requests stem ];
      doCheck = false;
      meta = with lib; {
        homepage = "http://github.com/erdiaker/torrequest";
        license = licenses.mit;
      };
    };
  };
}
