{ lib, config, ... }:

let
  inherit (config.lib.xcompose) toKeys;
in {
  xserver.xcompose = {
    enable = true;
    includeLocale = true;
    mappings = [
      # Characters
      { from = toKeys "0 ";      to = "​"; }
      { from = toKeys " 0";      to = "​"; }
      { from = toKeys "~-";      to = "～"; }
      { from = toKeys "-~";      to = "～"; }
      # Math
      { from = toKeys "<_>";     to = "↔"; } # Note: breaks "<_" -> "≤"
      { from = toKeys "==";      to = "⇔"; }
      { from = toKeys "<=";      to = "⇐"; } # Normally "<=" would become "≤", but we can use the synonym "_<" instead
      { from = toKeys "--o";     to = "⊸"; }
      { from = toKeys "/_=";     to = "≢"; }
      { from = toKeys "(-";      to = "∈"; }
      { from = toKeys "/(-";     to = "∉"; }
      { from = toKeys ")-";      to = "∋"; }
      { from = toKeys "/)-";     to = "∌"; }
      { from = toKeys "C(";      to = "⊂"; }
      { from = toKeys "/C(";     to = "⊄"; }
      { from = toKeys "C)";      to = "⊃"; }
      { from = toKeys "/C)";     to = "⊅"; }
      { from = toKeys "(_";      to = "⊆"; }
      { from = toKeys "/(_";     to = "⊈"; }
      { from = toKeys ")_";      to = "⊇"; }
      { from = toKeys "/)_";     to = "⊉"; }
      { from = toKeys ")N";      to = "⋂"; }
      { from = toKeys ")U";      to = "⋃"; }
      { from = toKeys "forall";  to = "∀"; }
      { from = toKeys "exists";  to = "∃"; }
      { from = toKeys "/exists"; to = "∄"; }
      { from = toKeys "_|_";     to = "⊥"; }
      { from = toKeys "|-";      to = "⊢"; }
      { from = toKeys "()";      to = "∘"; }
      { from = toKeys "0+";      to = "⊕"; }
      { from = toKeys "0x";      to = "⊗"; }
      { from = toKeys "0*";      to = "⊛"; } # Breaks "0*" -> "°", but can use the synonym "oo" instead
      { from = toKeys "0.";      to = "⊙"; }
      { from = toKeys "0-";      to = "⊖"; }
      { from = toKeys "&&";      to = "∧"; }
      { from = toKeys "||";      to = "∨"; }
      # Emoticons
      { from = toKeys "SHRUG";   to = ''¯\_(ツ)_/¯''; }
      { from = toKeys "MSHRUG";  to = ''¯\\\_(ツ)\_/¯''; } # shrug suitably escaped for use markdown
      { from = toKeys "SQUID";   to = ''くコ:彡''; }
    ];
  };
}

