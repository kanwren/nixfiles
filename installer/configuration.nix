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
  nix = {
    package = pkgs.nixFlakes;
    extraOptions = ''
      experimental-features = nix-command flakes ca-references
    '';
  };
  services = {
    openssh = {
      enable = true;
      permitRootLogin = "yes";
    };
  };
  users.users.root.password = "nixos";
  environment = {
    variables.EDITOR = "vim";
    systemPackages = with pkgs; [
      git
      tmux
      (pkgs.vim_configurable.customize {
        name = "vim";
        vimrcConfig = {
          packages.vim-package-group.start = with pkgs.vimPlugins; [ vim-nix vim-surround ];
          customRC = builtins.readFile ./minimal.vimrc;
        };
      })
    ];
  };
  programs.bash.interactiveShellInit = ''
    set -o vi
  '';
}
