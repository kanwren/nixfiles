{ pkgs, lib, config, ... }:

let
  sources = import ../../nix/sources.nix;
  std = import sources.nix-std;

  piazza-slackbot = import ./piazza-slackbot.nix { inherit pkgs std; } {
    inherit (cfg)
      piazza_id piazza_email piazza_password slack_token channel bot_name;
  };
  cfg = config.services.piazza-slackbot;
in {
  options.services.piazza-slackbot = {
    enable = lib.mkEnableOption "Enable Piazza Slackbot";
    piazza_id = lib.mkOption {
      type = lib.types.str;
      description = "The suffix of the Piazza URL";
    };
    piazza_email = lib.mkOption {
      type = lib.types.str;
      description = "Your Piazza email";
    };
    piazza_password = lib.mkOption {
      type = lib.types.str;
      description = "Your Piazza password";
    };
    slack_token = lib.mkOption {
      type = lib.types.str;
      description = "Slack bot token";
    };
    channel = lib.mkOption {
      type = lib.types.str;
      description = "The Slack channel to post in";
    };
    bot_name = lib.mkOption {
      type = lib.types.str;
      description = "The username of the Slackbot";
    };
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ piazza-slackbot ];
    systemd.user.services.piazza-slackbot = {
      description = "Piazza Bot";
      wantedBy = [ "default.target" ];
      serviceConfig = {
        ExecStart = ''
          ${piazza-slackbot}/bin/piazza-slackbot
        '';
        Restart = "on-failure";
        RestartSec = 3;
        RestartPreventExitStatus = 3;
      };
    };
  };
}
