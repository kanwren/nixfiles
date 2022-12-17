let
  lines = s: builtins.filter (x: builtins.isString x && x != "") (builtins.split "\n" s);

  keyfile = builtins.fetchurl {
    url = "https://github.com/kanwren.keys";
    sha256 = "1a7gja4yk2s0bp2fbvbh4hwgci3cbg99jp9b784q54rnqyfvbzxs";
  };
in
lines (builtins.readFile keyfile)
