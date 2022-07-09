{ fetchFromGitHub
, runCommand
}:

let
  src = fetchFromGitHub {
    owner = "catppuccin";
    repo = "spicetify";
    rev = "9b2170628eeab74b4582be4df646f4739f5e5b0a";
    sha256 = "0n1bzpcn5fgzjg6vq0wp3rrr9vw3kydhbymh31j6amp9vgf5a2gn";
  };
in

runCommand "catppuccin-spicetify" { } ''
  THEME_DIR=$out/share/spicetify/Themes/catppuccin
  EXT_DIR=$out/share/spicetify/Extensions

  mkdir -p $THEME_DIR $EXT_DIR

  cp ${src}/catppuccin.js $EXT_DIR
  cp ${src}/user.css ${src}/color.ini $THEME_DIR
''
