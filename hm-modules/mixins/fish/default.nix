{
  pkgs,
  config,
  lib,
  ...
}:
let
  cfg = config.mixins.fish;
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
    ];

    programs.fish = {
      enable = true;

      interactiveShellInit = /* fish */ ''
        # use vi bindings
        fish_vi_key_bindings
      '';

      functions =
        let
          aws = "${pkgs.awscli2}/bin/aws";
          fzf = "${pkgs.fzf}/bin/fzf";
          jq = "${pkgs.jq}/bin/jq";
        in
        {
          fish_prompt = {
            description = "a minimal prompt";
            body = /* fish */ ''
              set --local last_status $status
              if set -q SSH_TTY
                prompt_login
                printf ' '
              end
              test $last_status = 0; and set_color --bold green; or set_color --bold red
              printf '$'
              set_color normal
              printf ' '
            '';
          };

          fish_right_prompt = {
            description = "show cwd on right";
            body = /* fish */ ''
              set_color $fish_color_cwd
              prompt_pwd
              set_color normal
            '';
          };

          fish_mode_prompt = {
            description = "no mode prompt";
            body = "";
          };

          fish_greeting = {
            description = "no greeting";
            body = "";
          };

          # Misc shell utilities

          "yield" = {
            description = "Yield the arguments";
            body = /* fish */ ''
              if test (count $argv) -gt 0
                printf '%s\0' $argv | string split0
              end
            '';
          };

          dump = {
            description = "Quote each argument for fish and present the results like a command";
            body = /* fish */ ''
              string join -- ' ' (string escape --style=script -- $argv)
              return 0
            '';
          };

          # AWS CLI utility functions

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

          # Abbreviation expansion functions

          _pfor_expand = {
            description = "Expand a pfor command";
            body = /* fish */ ''
              set varname (string split --no-empty -- '.' (string replace --regex '.*?\\.' ''' $argv[1]))
              set result 'parallel'
              if test (count $varname) -gt 1
                set result "$result"' -C'"'"' '"'"
              end
              set result "$result"' -j10 -kq fish -c '"'"
              for i in (seq 1 (count $varname))
                set result "$result"'set '$varname[$i]' $argv['$i']; '
              end
              set result "$result%'"
              if test (count $varname) -gt 1
                for i in (seq 1 (count $varname))
                  set result "$result"' {'$i'}'
                end
              else
                  set result "$result"' {}'
              end
              printf '%s' "$result"
            '';
          };

          _expand_seq = {
            description = "Expand {start..end} to (seq start end)";
            body = /* fish */ ''
              set nums (string match -rg '(.*)\\{(\\d+)\\.\\.(\\d+)\\}(.*)' $argv[1])
              printf '%s(seq %d %d)%s' $nums
            '';
          };

          _expand_which = {
            description = "Expand =foo to the path to foo";
            body = /* fish */ ''
              if test (string sub --end 2 $argv[1]) = '=='
                realpath (command --search (string sub --start 3 $argv[1]))
              else
                command --search (string sub --start 2 $argv[1])
              end
            '';
          };

          _expand_last_command = {
            description = "Expand the last command";
            body = /* fish */ ''
              echo $history[1]
            '';
          };

          _expand_multidotdot = {
            description = "Expand .n to n directories up";
            body = /* fish */ ''
              set arg $argv[1]
              for ixs in (string match --index --regex '(?<![^/])\.[1-9]\d*(?![^/])' -- $arg)
                  set ix (string split -- ' ' $ixs)
                  set start $ix[1]
                  set length $ix[2]
                  set prefix '''
                  test (math $start) -gt 1; and set prefix (string sub --end (math $start - 1) -- $arg)
                  set suffix (string sub --start (math $start + $length) -- $arg)
                  set nparts (string sub --start (math $start + 1) --length (math $length - 1) -- $arg)
                  set dots (string repeat --count $nparts ../)
                  set arg "$prefix$dots$suffix"
              end
              printf '%s!' $arg
            '';
          };
        };

      shellAbbrs = lib.attrsets.mergeAttrsList (
        builtins.attrValues (
          let
            commandAbbr = cmd: {
              position = "command";
              expansion = cmd;
            };
          in
          {
            syntaxAbbrs = {
              "expand_seq_abbr" = {
                position = "anywhere";
                function = "_expand_seq";
                regex = ''.*\{\d+\.\.\d+\}.*'';
              };
              "expand_which_abbr" = {
                position = "anywhere";
                function = "_expand_which";
                regex = ''==?\S+'';
              };
              "!!" = {
                position = "anywhere";
                function = "_expand_last_command";
              };
              "multidotdot_abbr" = {
                position = "anywhere";
                function = "_expand_multidotdot";
                regex = ''(?<![^/])\.[1-9]\d*(?![^/])'';
                setCursor = "!";
              };
              "pfor" = {
                position = "command";
                function = "_pfor_expand";
                regex = ''pfor\..+'';
                setCursor = "%";
              };
            };
            findAbbrs = {
              "find1" = {
                position = "command";
                setCursor = "%";
                expansion = "find % -mindepth 1 -maxdepth 1";
              };
              "-sh" = {
                command = "find";
                position = "anywhere";
                setCursor = "%";
                expansion = "-exec sh -c 'x=\"$1\"; %' -- {} ';'";
              };
            };
            bazelAbbrs = {
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
            gitAbbrs = {
              "g" = commandAbbr "git";
              "ga" = commandAbbr "git add";
              "gb" = commandAbbr "git branch";
              "gc" = commandAbbr "git commit";
              "gd" = commandAbbr "git diff";
              "gl" = commandAbbr "git log";
              "gp" = commandAbbr "git pull";
              "gpf" = commandAbbr "git push --force-with-lease";
              "gr" = commandAbbr "git rebase";
              "gra" = commandAbbr "git rebase --abort";
              "grc" = commandAbbr "git rebase --continue";
              "gro" = commandAbbr "git rebase --onto";
              "gri" = commandAbbr "git rebase --interactive";
              "grs" = commandAbbr "git restore";
              "gsh" = commandAbbr "git show";
              "gst" = commandAbbr "git status";
              "gsw" = commandAbbr "git switch";
              "gswc" = commandAbbr "git switch --create";
              "gswd" = commandAbbr "git switch --detach";
              "gx" = commandAbbr "git reset";
              "gxm" = commandAbbr "git reset --mixed";
              "gxh" = commandAbbr "git reset --hard";
              "gxs" = commandAbbr "git reset --soft";
            };
            goAbbrs = {
              "gob" = commandAbbr "go build";
              "gog" = commandAbbr "go get";
              "goi" = commandAbbr "go install";
              "gom" = commandAbbr "go mod";
              "gomi" = commandAbbr "go mod init";
              "gomt" = commandAbbr "go mod tidy";
              "gor" = commandAbbr "go run";
              "got" = commandAbbr "go test";
            };
            jjAbbrs = {
              "-T" = {
                command = "jj";
                position = "anywhere";
                setCursor = "%";
                expansion = "--no-graph --template '%'";
              };
              "j/" = commandAbbr "jj split";
              "j/p" = commandAbbr "jj split --parallel";
              "ja" = commandAbbr "jj absorb";
              "jb" = commandAbbr "jj bookmark";
              "jbc" = commandAbbr "jj bookmark create";
              "jbd" = commandAbbr "jj bookmark delete";
              "jbf" = commandAbbr "jj bookmark forget";
              "jbl" = commandAbbr "jj bookmark list";
              "jbm" = commandAbbr "jj bookmark move";
              "jbn" = commandAbbr "jj bookmark-names";
              "jbr" = commandAbbr "jj bookmark rename";
              "jbs" = commandAbbr "jj bookmark set";
              "jbt" = commandAbbr "jj bookmark track";
              "jbu" = commandAbbr "jj bookmark untrack";
              "jc" = commandAbbr "jj commit";
              "jcf" = commandAbbr "jj config";
              "jcfl" = commandAbbr "jj config list";
              "jci" = commandAbbr "jj commit --interactive";
              "jcm" = commandAbbr "jj commit --message";
              "jd" = commandAbbr "jj describe";
              "jde" = commandAbbr "jj diffedit";
              "jdf" = commandAbbr "jj diff";
              "jdup" = commandAbbr "jj duplicate";
              "je" = commandAbbr "jj edit";
              "jf" = commandAbbr "jj file";
              "jfa" = commandAbbr "jj file annotate";
              "jfc" = commandAbbr "jj file chmod";
              "jfl" = commandAbbr "jj file list";
              "jfs" = commandAbbr "jj file show";
              "jft" = commandAbbr "jj file track";
              "jfu" = commandAbbr "jj file untrack";
              "jg" = commandAbbr "jj git";
              "jgf" = commandAbbr "jj git fetch";
              "jgi" = commandAbbr "jj git init";
              "jgp" = commandAbbr "jj git push";
              "jid" = commandAbbr "jj id";
              "jk" = commandAbbr "jj abandon";
              "jl" = commandAbbr "jj log";
              "jlr" = commandAbbr "jj log --revisions";
              "jm" = commandAbbr "jj metaedit";
              "jmc" = commandAbbr "jj metaedit --update-change-id";
              "jmm" = commandAbbr "jj mm";
              "jmmca" = commandAbbr "jj mm changes add";
              "jmmcc" = commandAbbr "jj mm changes clean-empty";
              "jmmcm" = commandAbbr "jj mm changes move";
              "jmmcr" = commandAbbr "jj mm changes remove";
              "jmmp" = commandAbbr "jj mm push";
              "jmmr" = commandAbbr "jj mm rebase";
              "jmmrt" = commandAbbr "jj mm rebase 'trunk()'";
              "jmmt" = commandAbbr "jj mm tip";
              "jn" = commandAbbr "jj new";
              "jna" = commandAbbr "jj new --no-edit --insert-after";
              "jnb" = commandAbbr "jj new --no-edit --insert-before";
              "jne" = commandAbbr "jj new --no-edit";
              "jnt" = commandAbbr "jj new 'trunk()'";
              "joa" = commandAbbr "jj operation abandon";
              "jod" = commandAbbr "jj operation diff";
              "jol" = commandAbbr "jj operation log";
              "jop" = commandAbbr "jj operation";
              "jor" = commandAbbr "jj operation restore";
              "jos" = commandAbbr "jj operation show";
              "jox" = commandAbbr "jj operation revert";
              "jr" = commandAbbr "jj rebase";
              "jra" = commandAbbr "jj rebase --insert-after";
              "jrb" = commandAbbr "jj rebase --insert-before";
              "jrt" = commandAbbr "jj rebase --destination 'trunk()'";
              "js" = commandAbbr "jj show";
              "jsp" = commandAbbr "jj simplify-parents";
              "jspr" = commandAbbr "jj simplify-parents --revisions";
              "jsq" = commandAbbr "jj squash";
              "jt" = commandAbbr "jj tag";
              "jtl" = commandAbbr "jj tag list";
              "ju" = commandAbbr "jj undo";
              "jv" = commandAbbr "jj parallelize";
              "jw" = commandAbbr "jj workspace";
              "jwa" = commandAbbr "jj workspace add";
              "jwf" = commandAbbr "jj workspace forget";
              "jwl" = commandAbbr "jj workspace list";
              "jwr" = commandAbbr "jj workspace rename";
              "jwu" = commandAbbr "jj workspace update-stale";
              "jx" = commandAbbr "jj revert";
              "jz" = commandAbbr "jj restore";
              "jzd" = commandAbbr "jj restore --restore-descendants";
              "jzi" = commandAbbr "jj restore --interactive";
            };
            zellijAbbrs = {
              "zj" = commandAbbr "zellij";
              "zjr" = commandAbbr "zellij run --";
              "zje" = commandAbbr "zellij edit";
              "zjl" = commandAbbr "zellij list-sessions";
              "zja" = commandAbbr "zellij attach";
              "zjk" = commandAbbr "zellij kill-session";
              "zjd" = commandAbbr "zellij delete-all-sessions";
            };
          }
        )
      );
    };
  };
}
