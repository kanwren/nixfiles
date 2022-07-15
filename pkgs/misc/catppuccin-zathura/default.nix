{ fetchFromGitHub
, runCommand
}:

let
  src = fetchFromGitHub {
    owner = "catppuccin";
    repo = "zathura";
    rev = "a7f22e5bd8acf5b2a9760d13e33bbbeab1d204f0";
    sha256 = "0bxld5yqw538s8wan97s3xvhyh5m9xr75gzrbh1hzn4kxzqz3yd9";
  };
in

runCommand "catppuccin-zathura" { } ''
  mkdir -p $out/share/zathura/themes
  cp ${src}/src/* $out/share/zathura/themes
''
