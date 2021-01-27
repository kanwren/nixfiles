{ lib, ... }:

let
  idPairs = xs: lib.listToAttrs (builtins.map (x: { name = x; value = x; }) xs);
  numbers = idPairs (builtins.map builtins.toString (lib.range 0 9));
  letters = idPairs (lib.stringToCharacters "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ");
  symbols = {
    "-" = "minus";
    " " = "space";
    "<" = "less";
    ">" = "greater";
    "=" = "equal";
    "_" = "underscore";
    "(" = "parenleft";
    ")" = "parenright";
    "/" = "slash";
    "\\" = "backslash";
    "." = "period";
    "," = "comma";
    "|" = "bar";
    "~" = "asciitilde";
    "*" = "asterisk";
    "+" = "plus";
    "&" = "ampersand";
  };
  common = letters // numbers // symbols;
  toKeys = str: builtins.map (x: common.${x}) (lib.stringToCharacters str);
in {
  xserver.xcompose = {
    enable = true;
    includeLocale = true;
    mappings = [
      # Characters
      { keys = toKeys "0 "; result = "​"; }
      { keys = toKeys " 0"; result = "​"; }
      { keys = toKeys "~-"; result = "～"; }
      { keys = toKeys "-~"; result = "～"; }
      # Math
      { keys = toKeys "<_>";     result = "↔"; } # Note: breaks "<_" -> "≤"
      { keys = toKeys "==";      result = "⇔"; }
      { keys = toKeys "<=";      result = "⇐"; } # Normally "<=" would become "≤", but we can use the synonym "_<" instead
      { keys = toKeys "--o";     result = "⊸"; }
      { keys = toKeys "/_=";     result = "≢"; }
      { keys = toKeys "(-";      result = "∈"; }
      { keys = toKeys "/(-";     result = "∉"; }
      { keys = toKeys ")-";      result = "∋"; }
      { keys = toKeys "/)-";     result = "∌"; }
      { keys = toKeys "(C";      result = "⊂"; }
      { keys = toKeys "/(C";     result = "⊄"; }
      { keys = toKeys ")C";      result = "⊃"; }
      { keys = toKeys "/)C";     result = "⊅"; }
      { keys = toKeys "(_";      result = "⊆"; }
      { keys = toKeys "/(_";     result = "⊈"; }
      { keys = toKeys ")_";      result = "⊇"; }
      { keys = toKeys "/)_";     result = "⊉"; }
      { keys = toKeys ")U";      result = "⋂"; }
      { keys = toKeys "(U";      result = "⋃"; }
      { keys = toKeys "forall";  result = "∀"; }
      { keys = toKeys "exists";  result = "∃"; }
      { keys = toKeys "/exists"; result = "∄"; }
      { keys = toKeys "_|_";     result = "⊥"; }
      { keys = toKeys "|-";      result = "⊢"; }
      { keys = toKeys "()";      result = "∘"; }
      { keys = toKeys "0+";      result = "⊕"; }
      { keys = toKeys "0x";      result = "⊗"; }
      { keys = toKeys "0*";      result = "⊛"; } # Breaks "0*" -> "°", but can use the synonym "oo" instead
      { keys = toKeys "0.";      result = "⊙"; }
      { keys = toKeys "&&";      result = "∧"; }
      { keys = toKeys "||";      result = "∨"; }
      # Emoticons
      { keys = toKeys "SHRUG"; result = ''¯\_(ツ)_/¯''; }
      { keys = toKeys "MSHRUG"; result = ''¯\\\_(ツ)\_/¯''; } # shrug suitably escaped for markdown
      { keys = toKeys "SQUID"; result = ''くコ:彡''; }
    ];
  };
}

