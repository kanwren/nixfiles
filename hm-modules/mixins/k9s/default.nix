{ config, lib, pkgs, ... }:

let
  cfg = config.mixins.k9s;
in
{
  options.mixins.k9s = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = config.mixins.enable;
      description = "Whether to enable the k9s mixin";
    };
  };

  config = lib.mkIf cfg.enable {
    programs.k9s = {
      enable = true;
      plugin.plugins = {
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
                bashOptions = [ "errexit" "nounset" "pipefail" ];
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
}

