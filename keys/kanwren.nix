let
  lines = s: builtins.filter (x: builtins.isString x && x != "") (builtins.split "\n" s);

  keyfile = builtins.fetchurl {
    url = "https://github.com/kanwren.keys";
    sha256 = "0lciz4n4z4fc25lka7cvwpxwm6dpgrmh9nwr0q203y976mcfqpdx";
  };
in
lines (builtins.readFile keyfile)
