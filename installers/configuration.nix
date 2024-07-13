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

let
  my-vim = pkgs.vim_configurable.customize {
    name = "vim";
    vimrcConfig = {
      packages.vim-package-group.start = with pkgs.vimPlugins; [ vim-nix vim-surround ];
      customRC = builtins.readFile ./minimal.vimrc;
    };
  };
in
{
  system.stateVersion = "22.11";

  nix = {
    extraOptions = ''
      experimental-features = nix-command flakes ca-derivations
    '';
  };

  services = {
    openssh = {
      enable = true;
      settings = {
        PermitRootLogin = "yes";
      };
    };
  };

  networking.wireless.enable = true;

  programs = {
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
      pinentryPackage = pkgs.pinentry;
    };
    bash.completion.enable = true;
  };

  users.users.root = {
    password = "nixos";
    initialHashedPassword = lib.mkForce null;
  };

  environment = {
    variables.EDITOR = "vim";

    systemPackages = [
      my-vim
      (pkgs.writeShellScriptBin "locate-nixfiles" "echo ${../.}")

      pkgs.git
      pkgs.tmux
      pkgs.parted
      pkgs.unixtools.fdisk
      pkgs.ripgrep
      pkgs.fzf
      pkgs.wget
      pkgs.curl
      pkgs.gnupg
      pkgs.mkpasswd
    ];
  };

  programs.bash.interactiveShellInit = ''
    set -o vi
  '';
}
