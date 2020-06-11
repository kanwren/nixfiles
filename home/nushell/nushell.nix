{ config, lib, pkgs, ... }:

let
  cfg = config.programs.nushell;
in {
  options = {
    programs.nushell = {
      enable = lib.mkEnableOption "nushell";

      settings = lib.mkOption {
        type = lib.types.attrs;
        default = {};
        example = lib.literalExample ''
          {
            completion_mode = "list";
            ctrlc_exit = true;
            edit_mode = "vi";
            key_timeout = 0;
            pivot_mode = "never";
          }
        '';
      };

      description = ''
        Configuration written to <filename>~/.config/nu/config.toml</filename>.
        See available options at
        <link>https://github.com/nushell/nushell/blob/master/docs/commands/config.md</link>
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.nushell ];

    home.file.".config/nu/config.toml" = lib.mkIf (cfg.settings != {}) {
      source =
        let json = pkgs.writeText "nushell-config-json" (builtins.toJSON cfg.settings);
        in pkgs.runCommand "nushell-config-toml" {} ''
          ${pkgs.remarshal}/bin/json2toml ${json} -o "$out"
        '';
    };
  };
}
