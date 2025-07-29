{ config, ... }: {
  environment.persistence."/persist".directories = [ "/home/gwen" ];

  sops.secrets."gwen/hashed-password" = {
    sopsFile = ../secrets/gwen/hashed-password.txt;
    format = "binary";
    mode = "0440";
    neededForUsers = true;
  };

  users.users.gwen = {
    hashedPasswordFile = config.sops.secrets."gwen/hashed-password".path;
    isNormalUser = true;
    extraGroups = [
      "wheel"
    ];
    createHome = true;
    packages = [ ];
  };
}
