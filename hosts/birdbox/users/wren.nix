{ pkgs, ... }:

{
  users.users.wren = {
    initialPassword = "hunter2";
    isNormalUser = true;
    uid = 1000;
    extraGroups = [
      "wheel"
    ];
    createHome = true;
    shell = pkgs.fish;
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDxh+3AOORhMo7mivoG1ckW4iQaOBZGUtQsQMyjPDy/d"
    ];
    packages = [ ];
  };

  home-manager.users.wren = {
    home = {
      stateVersion = "25.05";
      sessionPath = [ "$HOME/bin" ];
      sessionVariables = {
        EDITOR = "nvim";
        VISUAL = "nvim";
      };
      packages = [ ];
      persistence."/persist/home/wren" = {
        allowOther = true;
        directories = [
          "Downloads"
          "Music"
          "Pictures"
          "Documents"
          "Videos"
          ".gnupg"
          ".ssh"
          ".local/share/keyrings"
          ".local/share/direnv"
        ];
        files = [ ];
      };
    };
  };
}
