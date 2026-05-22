{
  coreutils,
  gnumake,
  gawk,
  gnugrep,
  writeShellApplication,
}:

writeShellApplication {
  name = "list-make-targets";

  runtimeInputs = [
    coreutils
    gnumake
    gawk
    gnugrep
  ];

  bashOptions = [
    "errexit"
    "nounset"
    "pipefail"
  ];

  text = /* bash */ ''
    make -pRrq : 2>/dev/null \
      | awk -v RS= -F: '/(^|\n)# Files(\n|$)/,/(^|\n)# Finished Make data base/ {if ($1 !~ "^[#.]") {print $1}}' \
      | sort \
      | grep '^[[:alnum:]]'
  '';
}
