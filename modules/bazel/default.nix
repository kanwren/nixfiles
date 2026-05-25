_toplevel:

{
  flake.modules.homeManager.bazel =
    { pkgs, ... }:
    {
      home.packages = [
        pkgs.bazelisk
        pkgs.buildozer
        (pkgs.runCommandLocal "bazel-bazelisk-alias" { } ''
          mkdir -p "$out/bin"
          ln -s "${pkgs.bazelisk}/bin/bazelisk" "$out/bin/bazel"
        '')
      ];

      programs.fish =
        let
          commandAbbr = cmd: {
            position = "command";
            expansion = cmd;
          };
        in
        {
          shellAbbrs = {
            "baq" = commandAbbr "bazel aquery";
            "bb" = commandAbbr "bazel build";
            "bcq" = commandAbbr "bazel cquery";
            "bcqf" = commandAbbr "bazel cquery --output files";
            "bq" = commandAbbr "bazel query";
            "bqb" = commandAbbr "bazel query --output build";
            "bqlk" = commandAbbr "bazel query --output label_kind";
            "br" = commandAbbr "bazel run";
            "bt" = commandAbbr "bazel test";
          };
        };

      xdg.configFile."fish/completions/aws-iam-authenticator".source =
        pkgs.runCommand "aws-iam-authenticator-completions"
          {
            nativeBuildInputs = [ pkgs.aws-iam-authenticator ];
          }
          "aws-iam-authenticator completion fish > $out";
    };
}
