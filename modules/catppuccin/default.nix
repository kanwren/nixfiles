{ inputs, ... }:

{
  flake.modules.nixos.catppuccin =
    { pkgs, ... }:
    {
      imports = [ inputs.catppuccin.nixosModules.catppuccin ];

      catppuccin = {
        enable = true;
        autoEnable = false;

        flavor = "mocha";
        accent = "lavender";

        grub.enable = true;
        tty.enable = true;
      };

      services.xserver.displayManager.lightdm = {
        greeters.gtk = {
          theme = {
            name = "catppuccin-mocha-lavender-standard";
            package = pkgs.catppuccin-gtk.override {
              accents = [ "lavender" ];
              variant = "mocha";
            };
          };

          iconTheme = {
            name = "FairyWren_Dark";
            package = pkgs.fairywren;
          };
        };

        background = ../../desktop-backgrounds/hearts.png;
      };
    };

  flake.modules.homeManager.catppuccin =
    {
      pkgs,
      config,
      ...
    }:
    {
      imports = [ inputs.catppuccin.homeModules.catppuccin ];

      catppuccin = {
        enable = true;
        autoEnable = false;

        flavor = "mocha";
        accent = "lavender";

        # false options included for emphasis, when it has explicitly broken something
        bat.enable = true;
        btop.enable = true;
        cava.enable = true;
        eza.enable = true;
        firefox.enable = false;
        fish.enable = true;
        fzf.enable = true;
        ghostty.enable = true;
        k9s.enable = true;
        mpv.enable = false;
        swaylock.enable = true;
        tmux.enable = true;
        yazi.enable = false;
        zathura.enable = true;
        zellij.enable = true;

        cursors.enable = config.gtk.enable;
        gtk.icon.enable = config.gtk.enable;
      };

      gtk = {
        theme = {
          name = "catppuccin-mocha-lavender-standard";
          package = pkgs.catppuccin-gtk.override {
            accents = [ "lavender" ];
            variant = "mocha";
          };
        };
      };
    };
}
