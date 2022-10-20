{ runCommand
, catppuccin-spicetify-src
}:

let
  src = catppuccin-spicetify-src;
in

runCommand "catppuccin-spicetify" { } ''
  THEME_DIR=$out/share/spicetify/Themes/catppuccin
  EXT_DIR=$out/share/spicetify/Extensions

  mkdir -p $THEME_DIR $EXT_DIR

  cp ${src}/catppuccin.js $EXT_DIR
  cp ${src}/user.css ${src}/color.ini $THEME_DIR
''
