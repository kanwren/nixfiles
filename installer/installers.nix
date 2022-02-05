{ nixos-generators
, nixpkgs
}:

let
  mkInstaller = system: format: nixos-generators.nixosGenerate {
    pkgs = nixpkgs.legacyPackages.${system};
    modules = [ ./configuration.nix ];
    inherit format;
  };
in
{
  packages = {
    x86_64-linux.install-iso = mkInstaller "x86_64-linux" "install-iso";
    aarch64-linux.sd-aarch64-installer = mkInstaller "aarch64-linux" "sd-aarch64-installer";
  };
}

