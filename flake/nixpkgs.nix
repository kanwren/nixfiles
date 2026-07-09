{ inputs, config, ... }:

{
  flake.overlays.pkgs-unstable = _final: prev: {
    pkgs-unstable = inputs.nixpkgs-unstable.legacyPackages.${prev.stdenv.hostPlatform.system};
  };

  perSystem =
    { system, ... }:
    {
      _module.args.pkgs = import inputs.nixpkgs {
        inherit system;
        overlays = [
          inputs.fenix.overlays.default
          inputs.naersk.overlays.default

          config.flake.overlays.pkgs-unstable
          config.flake.overlays.default
        ];
      };
    };
}
