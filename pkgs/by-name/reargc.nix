# (Re)-process a shell script with [argc](https://github.com/sigoden/argc) to
# insert static argument parsing code, without a dependency on argc itself.

{
  argc,
  writeShellApplication,
}:

writeShellApplication {
  name = "reargc";

  runtimeInputs = [
    argc
  ];

  bashOptions = [
    "errexit"
    "nounset"
    "pipefail"
  ];

  text = /* bash */ ''
    if [ $# -ne 1 ]; then
      printf 'usage: %s <file>\n' "$0"
      exit 1
    fi

    argc --argc-build "$1" "$1"
  '';
}
