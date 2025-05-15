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
      (pkgs.fishPlugins.fzf-fish.overrideAttrs { doCheck = false; })
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

        bind -M insert \e\cb fzf_git_branch_widget
      '';

      functions =
        let
          aws = "${pkgs.awscli2}/bin/aws";
          fzf = "${pkgs.fzf}/bin/fzf";
          jq = "${pkgs.jq}/bin/jq";
        in
        {
          "=" = {
            description = "Yield the arguments";
            body = ''
              if test (count $argv) -gt 0
                printf '%s\n' $argv
              end
            '';
          };

          # Misc shell utilities
          "last_argument" = {
            description = "Get the last argument to the last command in the history";
            body = ''
              printf '%s' "$history[1]" | read --array --tokenize result
              printf '%s\n' "$result[-1]"
            '';
          };

          _pfor_expand = {
            description = "Expand a pfor command";
            body = ''
              set varname (string replace --regex '.*?\\.' ''' $argv[1])
              echo 'parallel -j10 -kq fish -c \'set '$varname' $argv[1]; %\' {}'
            '';
          };

          fzf_git_branch_widget = {
            description = "List and insert git branches";
            body = ''
              set -l commandline (__fzf_parse_commandline)
              set -l fzf_query $commandline[2]
              set -l prefix $commandline[3]

              set -l result
              begin
                set -lx FZF_DEFAULT_OPTS (__fzf_defaults "--reverse")
                set -lx FZF_DEFAULT_COMMAND "$FZF_CTRL_T_COMMAND"
                set -lx FZF_DEFAULT_OPTS_FILE ""
                set fzfcmd (__fzfcmd)

                set refs (git for-each-ref refs/heads refs/remotes --exclude='refs/remotes/*/HEAD' --format='%(refname:lstrip=2)')
                or return 1

                printf '%s\n' $refs \
                  | $fzfcmd --query "$fzf_query" \
                  | while read -l sel; set -a result $sel; end
              end

              if test (count $result) -eq 0
                commandline -f repaint
                return
              end

              # Remove last token from commandline.
              commandline -t ""

              for i in $result
                commandline -it -- $prefix
                commandline -it -- (string escape $i)
                commandline -it -- ' '
              end

              commandline -f repaint
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

      shellAbbrs = lib.attrsets.mergeAttrsList (builtins.attrValues (
        let
          commandAbbrs = lib.attrsets.mapAttrs (k: v: { position = "command"; expansion = v; });
        in
        {
          miscAbbrs = {
            ",," = {
              position = "anywhere";
              function = "last_argument";
            };
            "pfor" = {
              position = "command";
              function = "_pfor_expand";
              regex = ''pfor\..+'';
              setCursor = "%";
            };
          };
          bazelAbbrs = commandAbbrs {
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
          gitAbbrs = commandAbbrs {
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
          goAbbrs = commandAbbrs {
            "gob" = "go build";
            "gog" = "go get";
            "goi" = "go install";
            "gom" = "go mod";
            "gomi" = "go mod init";
            "gomt" = "go mod tidy";
            "gor" = "go run";
            "got" = "go test";
          };
          kittyAbbrs = commandAbbrs {
            "kssh" = "kitten ssh";
            "icat" = "kitten icat --align=left";
          };
          kubectlAbbrs = commandAbbrs {
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
          jjAbbrs = commandAbbrs {
            "j/" = "jj split";
            "j/p" = "jj split --parallel";
            "ja" = "jj absorb";
            "jb" = "jj bookmark";
            "jbc" = "jj bookmark create";
            "jbd" = "jj bookmark delete";
            "jbf" = "jj bookmark forget";
            "jbl" = "jj bookmark list";
            "jbm" = "jj bookmark move";
            "jbr" = "jj bookmark rename";
            "jbs" = "jj bookmark set";
            "jbt" = "jj bookmark track";
            "jbu" = "jj bookmark untrack";
            "jc" = "jj commit";
            "jcf" = "jj config";
            "jcfl" = "jj config list";
            "jd" = "jj describe";
            "jde" = "jj diffedit";
            "jdf" = "jj diff";
            "jdup" = "jj duplicate";
            "je" = "jj edit";
            "jf" = "jj file";
            "jfa" = "jj file annotate";
            "jfc" = "jj file chmod";
            "jfl" = "jj file list";
            "jfs" = "jj file show";
            "jft" = "jj file track";
            "jfu" = "jj file untrack";
            "jg" = "jj git";
            "jgc" = "jj git init --colocate";
            "jgf" = "jj git fetch";
            "jgi" = "jj git init";
            "jgp" = "jj git push";
            "jk" = "jj abandon";
            "jl" = "jj log";
            "jlr" = "jj log --revisions";
            "jm" = "jj simplify-parents";
            "jn" = "jj new";
            "jna" = "jj new --no-edit --insert-after";
            "jnb" = "jj new --no-edit --insert-before";
            "jne" = "jj new --no-edit";
            "jnt" = "jj new 'trunk()'";
            "jop" = "jj operation";
            "jor" = "jj operation restore";
            "jos" = "jj operation show";
            "jou" = "jj operation undo";
            "jr" = "jj rebase";
            "jra" = "jj rebase --insert-after";
            "jrb" = "jj rebase --insert-before";
            "jrt" = "jj rebase --destination 'trunk()'";
            "js" = "jj show";
            "jsq" = "jj squash";
            "jt" = "jj tag";
            "jtl" = "jj tag list";
            "jv" = "jj parallelize";
            "jw" = "jj flow";
            "jwca" = "jj flow changes add";
            "jwcm" = "jj flow changes move";
            "jwcr" = "jj flow changes remove";
            "jwp" = "jj flow push";
            "jwr" = "jj flow rebase";
            "jwrt" = "jj flow rebase 'trunk()'";
            "jwt" = "jj flow tip";
            "jx" = "jj revert";
            "jz" = "jj restore";
            "jzd" = "jj restore --restore-descendants";
          };
          zellijAbbrs = commandAbbrs {
            "zj" = "zellij";
            "zjr" = "zellij run --";
            "zje" = "zellij edit";
            "zjl" = "zellij list-sessions";
            "zja" = "zellij attach";
            "zjk" = "zellij kill-session";
            "zjd" = "zellij delete-all-sessions";
          };
        }
      ));
    };
  };
}
