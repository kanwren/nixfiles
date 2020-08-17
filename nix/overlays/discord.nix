self: super:

{
  inherit (import (builtins.fetchTarball https://github.com/NixOS/nixpkgs/archive/master.tar.gz) {
    config.allowUnfree = true;
  }) discord;
}
