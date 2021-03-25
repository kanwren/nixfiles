{ nlib }:

{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.xserver.xcompose;

  wrap = x: "<" + x + ">";
  renderMapping = { from, to }:
    let
      mappings = concatMapStringsSep " " wrap from;
      res = lib.escape [ "\"" "\\" ] to;
    in "<Multi_key> ${mappings} : \"${res}\"";
in
{
  options = {
    xserver.xcompose = {
      enable = mkEnableOption "automatic ~/.XCompose file";

      includeLocale = mkOption {
        type = types.bool;
        default = true;
        description = "Whether or not to include the default locale's XCompose configurations";
      };

      mappings = mkOption {
        type = types.listOf (nlib.types.object false {
          from = nlib.types.required (types.listOf types.str);
          to = nlib.types.required (types.strMatching "[^\n]*");
        });
        default = [];
        description = ''
          A list of pairs mapping keys inputted to the desired result.
          For example, '{ keys = [ "0" "0" ]; result = "°"; }' would generate
          the line '<Multi_key> <0> <0> : "°"' in ~/.XCompose
        '';
      };

      extraConfig = mkOption {
        type = types.lines;
        default = "";
        description = "Extra configuration appended to ~/.XCompose";
      };
    };
  };

  config = mkIf cfg.enable {
    home.file.".XCompose".text = ''
      ${optionalString cfg.includeLocale "include \"%L\""}

      ${concatMapStringsSep "\n" renderMapping cfg.mappings}

      ${cfg.extraConfig}
    '';

    lib.xcompose =
      let
        idPairs = xs: lib.listToAttrs (builtins.map (x: { name = x; value = x; }) xs);
        numbers = idPairs (builtins.map builtins.toString (lib.range 0 9));
        letters = idPairs (lib.stringToCharacters "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ");
        symbols = {
          "-" = "minus";
          " " = "space";
          "<" = "less";
          ">" = "greater";
          "=" = "equal";
          "_" = "underscore";
          "(" = "parenleft";
          ")" = "parenright";
          "/" = "slash";
          "\\" = "backslash";
          "." = "period";
          "," = "comma";
          "|" = "bar";
          "~" = "asciitilde";
          "*" = "asterisk";
          "+" = "plus";
          "&" = "ampersand";
        };
        common = letters // numbers // symbols;
        toKeys = str: builtins.map (x: common.${x}) (lib.stringToCharacters str);
      in {
        inherit toKeys;
      };
  };
}
