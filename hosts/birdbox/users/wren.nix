{
  pkgs,
  config,
  ...
}:
{
  sops.secrets."wren/hashed-password" = {
    sopsFile = ../secrets/wren/hashed-password.txt;
    format = "binary";
    mode = "0440";
    neededForUsers = true;
  };

  users.users.wren = {
    hashedPasswordFile = config.sops.secrets."wren/hashed-password".path;
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
          ".config/nix/nix.conf" # for extra signing keys, etc.
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
