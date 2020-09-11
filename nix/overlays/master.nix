# Overlay for packages that I always want to be the most recent version. For
# example, Discord will periodically completely block you from using the app
# until you update.

self: super:

let
  nixpkgs-master = builtins.fetchTarball https://github.com/NixOS/nixpkgs/archive/master.tar.gz;
  pkgs-master = import nixpkgs-master {
    config.allowUnfree = true;
  };
in {
  inherit (pkgs-master)
    discord
    slack
    teams;
}
