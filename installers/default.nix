{ pkgs
, nixos-generators
}:

let
  mkInstaller = system: format: nixos-generators.nixosGenerate {
    inherit pkgs format;
    modules = [ ./configuration.nix ];
  };
in
{
  install-iso = mkInstaller "x86_64-linux" "install-iso";
  sd-aarch64-installer = mkInstaller "aarch64-linux" "sd-aarch64-installer";
}
