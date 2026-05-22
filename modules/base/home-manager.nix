{
  inputs,
  ...
}:

{
  flake.modules = {
    nixos.base = {
      imports = [
        inputs.home-manager.nixosModules.home-manager
      ];

      home-manager = {
        useGlobalPkgs = true;
        useUserPackages = true;
      };
    };

    darwin.base = {
      imports = [
        inputs.home-manager.darwinModules.home-manager
      ];

      home-manager = {
        useGlobalPkgs = true;
        useUserPackages = true;
      };
    };
  };
}
