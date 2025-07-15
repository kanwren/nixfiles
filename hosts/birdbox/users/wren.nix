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
          "code"
          "Downloads"
          "Music"
          "Pictures"
          "Documents"
          "Videos"
          ".gnupg"
          ".ssh"
          ".terminfo"
          ".local/share/keyrings"
          ".local/share/direnv"
          ".local/share/zoxide"
          ".local/share/wd"
        ];
        files = [
          ".local/share/fish/fish_history"
          ".bash_history"
        ];
      };
    };

    mixins = {
      bash.enable = true;
      btop.enable = true;
      catppuccin.enable = true;
      direnv.enable = true;
      fish.enable = true;
      git.enable = true;
      gpg-agent.enable = true;
      h.enable = true;
      jq.enable = true;
      jujutsu.enable = true;
      nix.enable = true;
      zellij.enable = true;
      zoxide.enable = true;
    };

    programs.jujutsu.settings = {
      user.email = "nicole@wren.systems";
    };
  };
}
