{
  inputs,
  ...
}:

{
  imports = [
    inputs.make-shell.flakeModules.default
  ];

  perSystem.make-shells.default =
    { pkgs, ... }:
    {
      packages = [
        pkgs.just
      ];
    };
}
