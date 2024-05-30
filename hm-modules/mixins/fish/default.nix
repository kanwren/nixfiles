{ self }:

{ pkgs, lib, ... }:

let
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
  home.packages = [
    pkgs.fishPlugins.foreign-env
    pkgs.fishPlugins.fzf-fish
    self.packages.${pkgs.system}.wd-fish
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
        "last_argument" = {
          description = "Get the last argument to the last command in the history";
          body = ''
            printf '%s' "$history[1]" | read --array --tokenize result
            printf '%s\n' "$result[-1]"
          '';
        };
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
        "use-java" = {
          description = "Switch JAVA_HOME to the given Java version";
          body = ''
            set --local argc (count $argv)

            if test $argc -eq 0
              set --local java_version (path change-extension ''' (path basename /Library/Java/JavaVirtualMachines/jdk*.jdk) | string replace --regex '^jdk-?' ''' | sort -Vr | fzf)
              or return $status
              set --global --export JAVA_HOME (/usr/libexec/java_home --version $java_version)
            else if test $argc -eq 1
              set --local java_version $argv[1]
              set --global --export JAVA_HOME (/usr/libexec/java_home --version $java_version)
              printf 'using %s\n' $JAVA_HOME
            else
              printf 'usage: use-java [<version>]\n'
              return 1
            end
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
        bazelAbbrs = {
          "baq" = { position = "command"; expansion = "bazel aquery"; };
          "bb" = { position = "command"; expansion = "bazel build"; };
          "bcq" = { position = "command"; expansion = "bazel cquery"; };
          "bcqf" = { position = "command"; expansion = "bazel cquery --output files"; };
          "bq" = { position = "command"; expansion = "bazel query"; };
          "bqb" = { position = "command"; expansion = "bazel query --output build"; };
          "bqlk" = { position = "command"; expansion = "bazel query --output label_kind"; };
          "br" = { position = "command"; expansion = "bazel run"; };
          "bt" = { position = "command"; expansion = "bazel test"; };
        };
        gitAbbrs = {
          "g" = { position = "command"; expansion = "git"; };
          "ga" = { position = "command"; expansion = "git add"; };
          "gb" = { position = "command"; expansion = "git branch"; };
          "gc" = { position = "command"; expansion = "git commit"; };
          "gd" = { position = "command"; expansion = "git diff"; };
          "gl" = { position = "command"; expansion = "git log"; };
          "gp" = { position = "command"; expansion = "git pull"; };
          "gpf" = { position = "command"; expansion = "git push --force-with-lease"; };
          "gr" = { position = "command"; expansion = "git rebase"; };
          "gra" = { position = "command"; expansion = "git rebase --abort"; };
          "grc" = { position = "command"; expansion = "git rebase --continue"; };
          "gro" = { position = "command"; expansion = "git rebase --onto"; };
          "gri" = { position = "command"; expansion = "git rebase --interactive"; };
          "grs" = { position = "command"; expansion = "git restore"; };
          "gsh" = { position = "command"; expansion = "git show"; };
          "gst" = { position = "command"; expansion = "git status"; };
          "gsw" = { position = "command"; expansion = "git switch"; };
          "gswc" = { position = "command"; expansion = "git switch --create"; };
          "gswd" = { position = "command"; expansion = "git switch --detach"; };
          "gx" = { position = "command"; expansion = "git reset"; };
          "gxm" = { position = "command"; expansion = "git reset --mixed"; };
          "gxh" = { position = "command"; expansion = "git reset --hard"; };
          "gxs" = { position = "command"; expansion = "git reset --soft"; };
        };
        goAbbrs = {
          "gob" = { position = "command"; expansion = "go build"; };
          "gog" = { position = "command"; expansion = "go get"; };
          "goi" = { position = "command"; expansion = "go install"; };
          "gom" = { position = "command"; expansion = "go mod"; };
          "gomi" = { position = "command"; expansion = "go mod init"; };
          "gomt" = { position = "command"; expansion = "go mod tidy"; };
          "gor" = { position = "command"; expansion = "go run"; };
          "got" = { position = "command"; expansion = "go test"; };
        };
        kittyAbbrs = {
          "kssh" = { position = "command"; expansion = "kitten ssh"; };
          "icat" = { position = "command"; expansion = "kitten icat --align=left"; };
        };
        kubectlAbbrs = {
          "k" = { position = "command"; expansion = "kubectl"; };
          "kaf" = { position = "command"; expansion = "kubectl apply --filename"; };
          "kc" = { position = "command"; expansion = "kubectx"; };
          "kcc" = { position = "command"; expansion = "kubectl config current-context"; };
          "kcp" = { position = "command"; expansion = "kubectl cp"; };
          "kcu" = { position = "command"; expansion = "kubectl config unset current-context"; };
          "kd" = { position = "command"; expansion = "kubectl describe"; };
          "krm" = { position = "command"; expansion = "kubectl delete"; };
          "ked" = { position = "command"; expansion = "kubectl edit"; };
          "kg" = { position = "command"; expansion = "kubectl get"; };
          "kl" = { position = "command"; expansion = "kubectl logs"; };
          "kn" = { position = "command"; expansion = "kubens"; };
          "knc" = { position = "command"; expansion = "kubens --current"; };
          "knu" = { position = "command"; expansion = "kubectl config unset contexts.(kubectl config current-context).namespace"; };
          "kx" = { position = "command"; expansion = "kubectl exec --stdin=true --tty=true"; };
        };
      in
      lib.attrsets.mergeAttrsList [
        miscAbbrs
        bazelAbbrs
        gitAbbrs
        goAbbrs
        kittyAbbrs
        kubectlAbbrs
      ];
  };

  # completions
  xdg.configFile = lib.attrsets.mapAttrs'
    (name: value: {
      name = "fish/completions/${name}.fish";
      value.source = value;
    })
    {
      "use-java" = pkgs.writeText "use-java" ''
        complete --command use-java --no-files --keep-order --arguments "(path change-extension ''' (path basename /Library/Java/JavaVirtualMachines/jdk*.jdk) | string replace --regex '^jdk-?' ''' | sort --numeric-sort --reverse)"
      '';
      "docker" = "${pkgs.docker.src}/contrib/completion/fish/docker.fish";
    };
}
