{
  pkgs,
  lib,
  config,
  ...
}:
let
  cfg = config.programs.h;
in
{
  options.programs.h = {
    enable = lib.mkEnableOption "h";

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.h;
      description = "The h package to use";
    };

    codeRoot = lib.mkOption {
      type = lib.types.str;
      default = "$HOME/code";
      description = "Root for repositories managed by h";
    };
  };

  config = lib.mkIf cfg.enable {
    programs.bash.initExtra = ''
      eval "$("${cfg.package}/bin/h" --setup "${cfg.codeRoot}")"
      eval "$("${cfg.package}/bin/up" --setup)"
    '';

    programs.zsh.initExtra = ''
      eval "$("${cfg.package}/bin/h" --setup "${cfg.codeRoot}")"
      eval "$("${cfg.package}/bin/up" --setup)"
    '';

    programs.fish.interactiveShellInit = ''
      function h
          set --local target ("${cfg.package}/bin/h" --resolve "${cfg.codeRoot}" $argv)
          or return $status

          test $target = (pwd); and return

          printf '%s\n' $target
          cd $target
      end

      function up
          set --local target ("${cfg.package}/bin/up" $argv)
          or return $status

          test $target = (pwd); and return

          printf '%s\n' $target
          cd $target
      end
    '';
  };
}
