{ fetchFromGitHub
, runCommand
}:

let
  src = fetchFromGitHub {
    owner = "catppuccin";
    repo = "btop";
    rev = "3610a8c497ac6dc24d297a27cf5aff2d8f63f9d0";
    sha256 = "169f7hldk84mrsgfzawabrdij3jb31prmwh9z46mszwxjmwsl7ny";
  };
in

runCommand "catppuccin-btop.theme" { } ''
  mkdir -p "$out"/share/btop/themes
  cp "${src}/catppuccin.theme" "$out"/share/btop/themes
''

