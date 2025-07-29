{ inputs, ... }:
let
  inherit (inputs.nixpkgs) lib;

  stable = final: prev: {
    stable = inputs.nixpkgs-stable.legacyPackages.${final.stdenv.hostPlatform.system};
  };
in
{
  inherit stable;

  fixes = lib.composeExtensions stable (_final: prev: {
    inherit (prev.stable) open-webui;
  });

  additions = lib.composeManyExtensions [
    inputs.fenix.overlays.default
    inputs.naersk.overlays.default
    (final: _: import ../pkgs { inherit (final) pkgs; })
  ];
}
