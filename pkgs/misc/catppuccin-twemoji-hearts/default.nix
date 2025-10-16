{
  lib,
  runCommand,
  generate-heart-emoji,
}:
let
  catppuccin-mocha = {
    rosewater = "#f5e0dc";
    flamingo = "#f2cdcd";
    pink = "#f5c2e7";
    mauve = "#cba6f7";
    red = "#f38ba8";
    maroon = "#eba0ac";
    peach = "#fab387";
    yellow = "#f9e2af";
    green = "#a6e3a1";
    teal = "#94e2d5";
    sky = "#89dceb";
    sapphire = "#74c7ec";
    blue = "#89b4fa";
    lavender = "#b4befe";
    base = "#1e1e2e";
    mantle = "#181825";
    crust = "#11111b";
  };
in
runCommand "catppuccin-twemoji-hearts" { buildInputs = [ generate-heart-emoji ]; } ''
  mkdir -p "$out"/share/emoji
  ${
    let
      attrsToList =
        as:
        builtins.map (name: {
          inherit name;
          value = as.${name};
        }) (builtins.attrNames as);

      mkHeart = name: color: ''
        generate-heart-emoji ${lib.escapeShellArg color} --format png --outfile "$out"/share/emoji/heart_${name}.png
      '';
    in
    builtins.concatStringsSep "\n" (
      builtins.map (
        {
          name,
          value,
        }:
        mkHeart name value
      ) (attrsToList catppuccin-mocha)
    )
  }
''
