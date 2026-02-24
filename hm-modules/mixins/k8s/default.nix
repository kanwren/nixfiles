{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.mixins.k8s;
in
{
  options.mixins.k8s = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = config.mixins.enable;
      description = "Whether to enable the Kubernetes mixin";
    };

    k9s.enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Whether to enable k9s";
    };

    kubie.enable = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Whether to enable kubie";
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [
      pkgs.kubectl
      pkgs.kubernetes-helm
      pkgs.kustomize
      pkgs.kind
      pkgs.ctlptl
      pkgs.tilt
    ];

    programs = {
      kubie = lib.mkIf cfg.kubie.enable {
        enable = true;
        settings = {
          prompt = {
            fish_use_rprompt = true;
          };
        };
      };

      fish.shellAbbrs = lib.mkIf config.programs.fish.enable (
        lib.attrsets.mergeAttrsList [
          {
            "k" = "kubectl";
            "kaf" = "kubectl apply --filename";
            "kann" = "kubectl annotate";
            "kc" = "kubie ctx";
            "kcc" = "kubectl config current-context";
            "kcn" = "kubectl config set contexts.(kubectl config current-context).namespace";
            "kcnu" = "kubectl config unset contexts.(kubectl config current-context).namespace";
            "kcp" = "kubectl cp";
            "kcu" = "kubectl config unset current-context";
            "kd" = "kubectl describe";
            "kdel" = "kubectl delete";
            "kdf" = "kubectl diff";
            "kdff" = "kubectl diff --filename";
            "ke" = "kubectl edit";
            "kg" = "kubectl get";
            "kgj" = "kubectl get --output json";
            "kgw" = "kubectl get --output wide";
            "kgy" = "kubectl get --output yaml";
            "kl" = "kubectl logs";
            "klbl" = "kubectl label";
            "klf" = "kubectl logs --follow";
            "kn" = "kubie ns";
            "kpj" = "kubectl patch --type=json";
            "kpm" = "kubectl patch --type=merge";
            "kps" = "kubectl patch --type=strategic";
            "kro" = "kubectl rollout";
            "kror" = "kubectl rollout restart";
            "kros" = "kubectl rollout status";
            "krou" = "kubectl rollout undo";
            "ks" = "kubectl scale";
            "ksr" = "kubectl scale --replicas";
            "kx" = "kubectl exec --stdin=true --tty=true";
          }
        ]
      );

      k9s = lib.mkIf cfg.k9s.enable {
        enable = true;
        plugins.plugins = {
          editjson = {
            shortCut = "Shift-E";
            description = "Edit as JSON";
            scopes = [ "all" ];
            background = false;
            command = "${pkgs.kubectl}/bin/kubectl";
            args = [
              "--context"
              "$CONTEXT"
              "--namespace"
              "$NAMESPACE"
              "edit"
              "$RESOURCE_NAME.$RESOURCE_GROUP/$NAME"
              "--output"
              "json"
            ];
          };

          editsecret = {
            shortCut = "Shift-X";
            description = "Edit secret";
            scopes = [ "secrets" ];
            background = false;
            command =
              let
                k9s-edit-secret = pkgs.writeShellApplication {
                  name = "k9s-edit-secret";
                  runtimeInputs = [
                    pkgs.coreutils
                    pkgs.yq-go
                    pkgs.kubectl
                  ];
                  bashOptions = [
                    "errexit"
                    "nounset"
                    "pipefail"
                  ];
                  text = ''
                    declare context namespace resource
                    context="''${1?missing argument: context}"
                    namespace="''${2?missing argument: namespace}"
                    resource="''${3?missing argument: resource}"
                    declare -r context namespace resource

                    declare manifest
                    manifest="$(mktemp --suffix=.yaml)"
                    declare -r manifest

                    kubectl --context="''${context}" --namespace="''${namespace}" get "''${resource}" --output=yaml | yq '.data[] |= @base64d' > "''${manifest}"
                    # shellcheck disable=SC2046 # EDITOR typically allows passing flags via shell splitting
                    ''${KUBE_EDITOR:-''${EDITOR:-vi}} "''${manifest}"
                    yq '.data[] |= @base64' "''${manifest}" | kubectl --context="''${context}" --namespace="''${namespace}" apply --filename=-
                  '';
                };
              in
              "${k9s-edit-secret}/bin/k9s-edit-secret";
            args = [
              "$CONTEXT"
              "$NAMESPACE"
              "$RESOURCE_NAME.$RESOURCE_GROUP/$NAME"
            ];
          };
        };
      };
    };
  };
}
