{ runCommand
, fetchFromGitHub
}:

let
  src = fetchFromGitHub {
    owner = "catppuccin";
    repo = "kitty";
    rev = "ad38e5bb1b1ab04e7d2cf86ded289c455df62908";
    sha256 = "0vb5fkpxjyyj180wfc948c1qvndlcwv0mzmz0xdv7wdg7qj9v7hk";
  };
in

runCommand "catppuccin-kitty" { } ''
  mkdir -p "$out"
  cp ${src}/*.conf "$out"
''

