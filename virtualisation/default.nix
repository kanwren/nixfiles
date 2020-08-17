{ pkgs, ... }:

{
  virtualisation = {
    docker = {
      enable = true;
      autoPrune = {
        enable = true;
        dates = "weekly";
      };
    };
  };

  programs = {
    singularity.enable = true;
  };
}
