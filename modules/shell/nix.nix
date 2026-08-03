{ inputs, ... }:

{
  flake.modules.nixos.shell =
    { pkgs, ... }:
    {
      nixpkgs.overlays = [
        inputs.nix-index-database.overlays.nix-index
      ];

      programs.nix-index = {
        enable = true;
        package = pkgs.nix-index-with-small-db;
      };

      environment.systemPackages = [
        pkgs.comma-with-db # wrapper using nix-index-database's small db
      ];
    };

  flake.modules.homeManager.shell = {
    programs.nix-your-shell = {
      enable = true;
    };
  };
}
