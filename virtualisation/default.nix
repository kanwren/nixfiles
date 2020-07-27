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

  environment.systemPackages = with pkgs; [
    vagrant
  ];
}
