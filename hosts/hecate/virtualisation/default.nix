{ pkgs, ... }:

{
  virtualisation = {
    # Docker
    docker = {
      enable = true;
      autoPrune = {
        enable = true;
        dates = "weekly";
      };
    };
    # Podman
    podman.enable = true;
  };

  # Singularity
  programs.singularity.enable = true;
}
