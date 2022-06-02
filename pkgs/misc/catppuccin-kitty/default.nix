{ runCommand
, fetchFromGitHub
}:

let
  src = fetchFromGitHub {
    owner = "catppuccin";
    repo = "kitty";
    rev = "a1a2cb75038b20a93300562e540c3fe0a985f601";
    sha256 = "sha256-JVQYNv+EMangTWmiLjhNxnmJqPYgL9bKIYENSk1l2qY=";
  };
in

runCommand "catppuccin-kitty" { } ''
  mkdir -p "$out"
  cp ${src}/*.conf "$out"
''

