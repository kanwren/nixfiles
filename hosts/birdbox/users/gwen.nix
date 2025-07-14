{ ... }:

{
  environment.persistence."/persist".directories = [ "/home/gwen" ];

  users.users.gwen = {
    initialPassword = "hunter2";
    isNormalUser = true;
    extraGroups = [
      "wheel"
    ];
    createHome = true;
    packages = [ ];
  };
}
