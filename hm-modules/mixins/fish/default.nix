{ pkgs, config, lib, ... }:

let
  cfg = config.mixins.fish;

  # alias kubectx outputs to also be kubectl plugins
  kubectx-kube-plugin = (pkgs.runCommandNoCCLocal "kubectx-kube-plugin" { } ''
    mkdir -p "$out/bin"
    ln -s "${pkgs.kubectx}/bin/kubectx" "$out/bin/kubectx"
    ln -s "${pkgs.kubectx}/bin/kubectx" "$out/bin/kubectl-ctx"
    ln -s "${pkgs.kubectx}/bin/kubens" "$out/bin/kubens"
    ln -s "${pkgs.kubectx}/bin/kubens" "$out/bin/kubectl-ns"
  '');
in
{
  options.mixins.fish.enable = lib.mkOption {
    type = lib.types.bool;
    default = config.mixins.enable;
    description = "Whether to enable the fish mixin";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.fishPlugins.foreign-env
      pkgs.fishPlugins.fzf-fish
      pkgs.wd-fish
      kubectx-kube-plugin
    ];

    programs.fish = {
      enable = true;

      interactiveShellInit = ''
        # set prompt to '; '
        function fish_prompt
          test $status = 0; and set_color --bold green; or set_color --bold red
          printf '; '
          set_color normal
        end

        function fish_mode_prompt; end

        # don't greet
        function fish_greeting; end

        # use vi bindings
        fish_vi_key_bindings

        # restore last directory when new shell is opened
        set --query fish_most_recent_dir
        and test -d "$fish_most_recent_dir"
        and cd "$fish_most_recent_dir"

        function save_dir --on-variable PWD
          set -U fish_most_recent_dir $PWD
        end
      '';

      functions =
        let
          aws = "${pkgs.awscli2}/bin/aws";
          fzf = "${pkgs.fzf}/bin/fzf";
          jq = "${pkgs.jq}/bin/jq";
        in
        {
          # Misc shell utilities
          "last_argument" = {
            description = "Get the last argument to the last command in the history";
            body = ''
              printf '%s' "$history[1]" | read --array --tokenize result
              printf '%s\n' "$result[-1]"
            '';
          };

          # AWS CLI utility functions
          "asp" = {
            description = "Switch AWS profiles";
            body = ''
              set --local choice ("${aws}" configure list-profiles | "${fzf}")
              or return $status
              set --global --export AWS_PROFILE $choice
            '';
          };
          "assume" = {
            description = "Assume an AWS role for a profile";
            body = ''
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
            body = ''
              set --query AWS_PROFILE;           and set --erase AWS_PROFILE
              set --query AWS_ACCESS_KEY_ID;     and set --erase AWS_ACCESS_KEY_ID
              set --query AWS_SECRET_ACCESS_KEY; and set --erase AWS_SECRET_ACCESS_KEY
              set --query AWS_SESSION_TOKEN;     and set --erase AWS_SESSION_TOKEN
              return 0
            '';
          };
        };

      shellAbbrs =
        let
          miscAbbrs = {
            ",," = {
              position = "anywhere";
              function = "last_argument";
            };
            "lines" = {
              position = "command";
              expansion = "printf '%s\\n'";
            };
          };
          bazelAbbrs = lib.attrsets.mapAttrs (k: v: { position = "command"; expansion = v; }) {
            "baq" = "bazel aquery";
            "bb" = "bazel build";
            "bcq" = "bazel cquery";
            "bcqf" = "bazel cquery --output files";
            "bq" = "bazel query";
            "bqb" = "bazel query --output build";
            "bqlk" = "bazel query --output label_kind";
            "br" = "bazel run";
            "bt" = "bazel test";
          };
          gitAbbrs = lib.attrsets.mapAttrs (k: v: { position = "command"; expansion = v; }) {
            "g" = "git";
            "ga" = "git add";
            "gb" = "git branch";
            "gc" = "git commit";
            "gd" = "git diff";
            "gl" = "git log";
            "gp" = "git pull";
            "gpf" = "git push --force-with-lease";
            "gr" = "git rebase";
            "gra" = "git rebase --abort";
            "grc" = "git rebase --continue";
            "gro" = "git rebase --onto";
            "gri" = "git rebase --interactive";
            "grs" = "git restore";
            "gsh" = "git show";
            "gst" = "git status";
            "gsw" = "git switch";
            "gswc" = "git switch --create";
            "gswd" = "git switch --detach";
            "gx" = "git reset";
            "gxm" = "git reset --mixed";
            "gxh" = "git reset --hard";
            "gxs" = "git reset --soft";
          };
          goAbbrs = lib.attrsets.mapAttrs (k: v: { position = "command"; expansion = v; }) {
            "gob" = "go build";
            "gog" = "go get";
            "goi" = "go install";
            "gom" = "go mod";
            "gomi" = "go mod init";
            "gomt" = "go mod tidy";
            "gor" = "go run";
            "got" = "go test";
          };
          kittyAbbrs = lib.attrsets.mapAttrs (k: v: { position = "command"; expansion = v; }) {
            "kssh" = "kitten ssh";
            "icat" = "kitten icat --align=left";
          };
          kubectlAbbrs = lib.attrsets.mapAttrs (k: v: { position = "command"; expansion = v; }) {
            "k" = "kubectl";
            "kaf" = "kubectl apply --filename";
            "kc" = "kubectx";
            "kcc" = "kubectl config current-context";
            "kcp" = "kubectl cp";
            "kcu" = "kubectl config unset current-context";
            "kd" = "kubectl describe";
            "krm" = "kubectl delete";
            "ked" = "kubectl edit";
            "kg" = "kubectl get";
            "kl" = "kubectl logs";
            "kn" = "kubens";
            "knc" = "kubens --current";
            "knu" = "kubectl config unset contexts.(kubectl config current-context).namespace";
            "kx" = "kubectl exec --stdin=true --tty=true";
          };
          jjAbbrs = lib.attrsets.mapAttrs (k: v: { position = "command"; expansion = v; }) {
            "jc" = "jj commit";
            "jsp" = "jj split";
            "jsq" = "jj squash";
            "jab" = "jj abandon";
            "jbo" = "jj backout";
            "jb" = "jj branch";
            "jbm" = "jj branch move";
            "jbt" = "jj branch track";
            "jbl" = "jj branch list";
            "jbc" = "jj branch create";
            "jbd" = "jj branch delete";
            "jbr" = "jj branch rename";
            "jd" = "jj describe";
            "jdf" = "jj diff";
            "je" = "jj edit";
            "jf" = "jj file";
            "jfs" = "jj file show";
            "jfl" = "jj file list";
            "jfc" = "jj file chmod";
            "jn" = "jj new";
            "jne" = "jj new --no-edit";
            "jna" = "jj new --no-edit --insert-after";
            "jnb" = "jj new --no-edit --insert-before";
            "jg" = "jj git";
            "jgf" = "jj git fetch";
            "jgp" = "jj git push";
            "jl" = "jj log";
            "jlr" = "jj log --revisions";
            "jo" = "jj operation";
            "jop" = "jj opertaion";
            "jou" = "jj operation undo";
            "jor" = "jj operation restore";
            "jos" = "jj operation show";
            "jrs" = "jj restore";
            "jr" = "jj rebase";
            "jrb" = "jj rebase";
            "jsh" = "jj show";
            "js" = "jj show";
            "jt" = "jj tag";
            "jtl" = "jj tag list";
            "jus" = "jj unsquash";
          };
        in
        lib.attrsets.mergeAttrsList [
          miscAbbrs
          bazelAbbrs
          gitAbbrs
          goAbbrs
          jjAbbrs
          kittyAbbrs
          kubectlAbbrs
        ];
    };
  };
}
