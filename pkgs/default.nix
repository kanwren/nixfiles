{ pkgs }:
rec {
  catppuccin-twemoji-hearts = pkgs.callPackage ./misc/catppuccin-twemoji-hearts {
    inherit generate-heart-emoji;
  };

  wd-fish = pkgs.fishPlugins.callPackage ./misc/wd-fish { };

  # scripts
  lipsum = pkgs.callPackage ./scripts/lipsum { };
  generate-heart-emoji = pkgs.callPackage ./scripts/generate-heart-emoji { };

  # tools
  slides-full = pkgs.callPackage ./tools/slides { };
  envtpl = pkgs.callPackage ./tools/envtpl { };
  frum = pkgs.callPackage ./tools/frum { };
  tfenv = pkgs.callPackage ./tools/tfenv { };
  ts-l4-rproxy = pkgs.callPackage ./tools/ts-l4-rproxy { };

  # apps
  caddy-with-plugins = pkgs.callPackage ./apps/caddy-with-plugins { };

  # games
  ftb-server-installer = pkgs.callPackage ./games/ftb-server-installer { };
}
