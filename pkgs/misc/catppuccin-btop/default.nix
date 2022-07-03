{ fetchFromGitHub
, runCommand
}:

let
  src = fetchFromGitHub {
    owner = "catppuccin";
    repo = "btop";
    rev = "276b9a9d08ad70e9fb0e6a2f8a1b181209c79c6f";
    sha256 = "1iiwvi3sqlma4fiz11knn7hgl157f1b216ag8blkbmls0yfv4glp";
  };
in

runCommand "catppuccin-btop.theme" { } ''
  mkdir -p "$out"/share/btop/themes
  cp ${src}/*.theme "$out"/share/btop/themes
''

