{
  inputs,
  ...
}:

{
  imports = [
    inputs.treefmt-nix.flakeModule
  ];

  perSystem.treefmt = {
    projectRootFile = "flake.nix";

    programs = {
      deadnix.enable = true;
      nixfmt.enable = true;
      shfmt.enable = true;
      statix.enable = true;
      just.enable = true;
    };

    settings = {
      on-unmatched = "fatal";
      global.excludes = [
        ".envrc"
        ".editorconfig"
        ".gitignore"
      ];
    };
  };
}
