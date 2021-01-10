# Build using nixos-generators. For example:
#
# For example, if building an installer iso on an x86_64-linux system:
#
#   nix run 'github:nix-community/nixos-generators#nixos-generate' -- \
#     -f install-iso -c configuration.nix
#
# If building an installer sd image for aarch64 on an x86_64-linux system,
# set 'boot.binfmt.emulatedSystems = [ "aarch64-linux" ];' and run:
#
#   nix run 'github:nix-community/nixos-generators#nixos-generate' -- \
#     -f sd-aarch64-installer --system aarch64-linux -c configuration.nix

{ pkgs, lib, ... }:

{
  services = {
    openssh = {
      enable = true;
      permitRootLogin = "yes";
    };
  };
  users.users.root.password = "nixos";
  environment.systemPackages = with pkgs; [
    git
  ];
}
