{ ... }:

{
  boot.wsl = {
    enable = true;
    user = "nprin";
  };

  users = {
    mutableUsers = true;
    users.nprin = {
      initialPassword = "setup";
    };
  };
}
