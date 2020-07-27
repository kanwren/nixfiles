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

    virtualbox.host = {
      enable = true;
    };
  };

  programs = {
    singularity.enable = true;
  };

  environment.systemPackages = with pkgs; [
    vagrant
  ];
}
