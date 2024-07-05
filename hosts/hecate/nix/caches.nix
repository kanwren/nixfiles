{
  nix.settings = {
    # cache.nixos.org is included by default
    substituters = [
      "https://kanwren.cachix.org"
      "https://cache.lix.systems"
    ];
    trusted-public-keys = [
      "kanwren.cachix.org-1:uMS7ZtVOdof/PU46BAyehmNDD/P6qCGhYEvYP7X8YfE="
      "cache.lix.systems:aBnZUw8zA7H35Cz2RyKFVs3H4PlGTLawyY5KRbvJR8o="
    ];
  };
}
