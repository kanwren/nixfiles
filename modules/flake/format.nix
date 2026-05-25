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
      just.enable = true;
      kdlfmt.enable = true;
      mdformat.enable = true;
      nixfmt.enable = true;
      oxipng.enable = true;
      prettier.enable = true;
      statix.enable = true;
      shfmt = {
        enable = true;
        useEditorConfig = true;
      };
    };

    settings = {
      on-unmatched = "fatal";
      global.excludes = [
        ".editorconfig"
        ".gitignore"
        "*.txt"
        "*.conf"
        "*.jpg"
      ];
    };
  };
}
