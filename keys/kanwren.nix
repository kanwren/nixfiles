let
  lines = s: builtins.filter (x: builtins.isString x && x != "") (builtins.split "\n" s);

  keyfile = builtins.fetchurl {
    url = "https://github.com/kanwren.keys";
    sha256 = "1fpchgi3phyja3gkf5g8xr2gzhnxzj737b3myyk5jqkkvq563r0i";
  };
in
lines (builtins.readFile keyfile)
