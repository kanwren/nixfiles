{ runCommand
, catppuccin-spicetify-src
}:

let
  src = catppuccin-spicetify-src;
in

runCommand "catppuccin-spicetify" { } ''
  THEME_DIR=$out/share/spicetify/Themes
  EXT_DIR=$out/share/spicetify/Extensions

  mkdir -p $THEME_DIR $EXT_DIR

  cp ${src}/js/* $EXT_DIR
  cp -r ${src}/catppuccin-* $THEME_DIR
''
