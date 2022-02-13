{ ... }:

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

    virtualbox.host.enable = true;
  };

  # Singularity
  programs.singularity.enable = true;
}
