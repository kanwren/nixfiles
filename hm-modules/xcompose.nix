{ nlib }:

{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.xserver.xcompose;

  wrap = x: "<" + x + ">";
  renderMapping = { keys, result }: "<Multi_key> ${concatMapStringsSep " " wrap keys} : \"${result}\"";
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
          keys = nlib.types.required (types.listOf types.str);
          result = nlib.types.required (types.str);
        });
        default = [];
        description = ''
          A list of pairs mapping keys inputted to the desired result.
          For example, '{ keys = [ "0" "0" ]; result = "°"; }' would generate
          the line '<Multi_key> <0> <0> : "°"' in ~/.XCompose
        '';
      };

      extraConfig = mkOption {
        type = types.str;
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
  };
}
