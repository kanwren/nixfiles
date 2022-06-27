{
  description = "nprindle's NixOS configurations and other Nix tools";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    # pinned from nixos-22.05-aarch64
    nixpkgs-homepi.url = "github:NixOS/nixpkgs/f919a40e544da31a3b4b42e87cf30a5078c2b09c";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixos-hardware.url = "github:NixOS/nixos-hardware";

    flake-utils.url = "github:numtide/flake-utils";

    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixos-wsl = {
      url = "github:nprindle/NixOS-WSL";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };

    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nvim-configs = {
      url = "github:nprindle/nvim-configs";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };
  };

  outputs =
    { self
    , nixpkgs
    , nixpkgs-homepi
    , nixos-hardware
    , flake-utils
    , nixos-wsl
    , sops-nix
    , home-manager
    , nixos-generators
    , nvim-configs
    , ...
    }@inputs:
    let
      # custom library
      nlib = import ./lib { inherit (nixpkgs) lib; };
    in
    nlib.attrsets.recursiveMergeAttrs [
      {
        nixosConfigurations = {
          # main dev laptop
          hecate = import ./hosts/hecate/host.nix inputs;

          homepi = import ./hosts/homepi/host.nix inputs;

          wsl = import ./hosts/wsl/host.nix inputs;
        };

        # Nixpkgs overlays
        overlays = import ./overlays;

        # NixOS modules
        nixosModules = import ./modules;

        # home-manager modules
        hmModules = {
          btop = import ./hm-modules/btop.nix;
        };

        # custom lib functions
        lib = nlib;

        # custom templates
        templates = import ./templates;
      }
      (flake-utils.lib.eachSystem [ "x86_64-linux" "aarch64-linux" ] (system: (
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          devShells.default = pkgs.mkShell {
            nativeBuildInputs = [
              # formatting
              pkgs.lefthook
              pkgs.nixpkgs-fmt
              # sops-nix
              sops-nix.packages.${system}.sops-import-keys-hook
              sops-nix.packages.${system}.ssh-to-pgp
              sops-nix.packages.${system}.sops-init-gpg-key
            ];
            sopsPGPKeyDirs = [
              "./secrets/keys/users"
            ];
          };

          packages = flake-utils.lib.flattenTree (import ./pkgs { inherit pkgs; });
        }
      )))
      (import ./installer/installers.nix {
        inherit nixpkgs nixos-generators;
      })
    ];
}
