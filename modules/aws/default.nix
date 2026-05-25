_toplevel:

{
  flake.modules.homeManager.aws =
    {
      pkgs,
      lib,
      ...
    }:
    {
      home.packages = [
        pkgs.awscli2
        pkgs.aws-iam-authenticator
        (pkgs.ssm-session-manager-plugin.overrideAttrs { doCheck = false; })
      ];

      programs.fish =
        let
          aws = lib.getExe' pkgs.awscli2 "aws";
          fzf = lib.getExe' pkgs.fzf "fzf";
          jq = lib.getExe' pkgs.jq "jq";
        in
        {
          functions = {
            "asp" = {
              description = "Switch AWS profiles";
              body = /* fish */ ''
                set --local choice ("${aws}" configure list-profiles | "${fzf}")
                or return $status
                set --global --export AWS_PROFILE $choice
              '';
            };

            "assume" = {
              description = "Assume an AWS role for a profile";
              body = /* fish */ ''
                set --query AWS_PROFILE
                and set --local profile "$AWS_PROFILE"
                or set profile ("${aws}" configure list-profiles | "${fzf}")
                or return $status

                set --local creds ("${aws}" configure export-credentials --profile "$profile")
                or begin
                  set --local ret $status
                  printf 'Failed to assume role for profile %s\n' (string escape "$profile")
                  return $ret
                end

                set --global --export AWS_ACCESS_KEY_ID     (echo $creds | "${jq}" --raw-output '.AccessKeyId')
                set --global --export AWS_SECRET_ACCESS_KEY (echo $creds | "${jq}" --raw-output '.SecretAccessKey')
                set --global --export AWS_SESSION_TOKEN     (echo $creds | "${jq}" --raw-output '.SessionToken')
                printf 'Assumed role for profile %s\n' (string escape "$profile")
              '';
            };

            "unassume" = {
              description = "Un-assume an AWS role";
              body = /* fish */ ''
                set --query AWS_PROFILE;           and set --erase AWS_PROFILE
                set --query AWS_ACCESS_KEY_ID;     and set --erase AWS_ACCESS_KEY_ID
                set --query AWS_SECRET_ACCESS_KEY; and set --erase AWS_SECRET_ACCESS_KEY
                set --query AWS_SESSION_TOKEN;     and set --erase AWS_SESSION_TOKEN
                return 0
              '';
            };
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
