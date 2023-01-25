{
  nix.settings = {
    # cache.nixos.org is included by default
    substituters = [ "https://kanwren.cachix.org" ];
    trusted-public-keys = [ "kanwren.cachix.org-1:uMS7ZtVOdof/PU46BAyehmNDD/P6qCGhYEvYP7X8YfE=" ];
  };
}
