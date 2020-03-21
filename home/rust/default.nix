{ pkgs, ... }:

{
  home-manager.users.nprin = {

    home.packages = (with pkgs; [
      rustc
      cargo
      rls
      rustfmt
    ]) ++ (with pkgs.rust.latest.rustChannels.stable; [
    ]);

  };
}

