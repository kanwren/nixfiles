{
  config,
  pkgs,
  lib,
  ...
}:
let
  intToString =
    n: if !builtins.isInt n then builtins.throw "intToString: expected an int" else toString n;
in
{
  services = {
    gitit = {
      enable = true;
      package =
        pkgs.stable.runCommand "gitit-stable" { nativeBuildInputs = [ pkgs.stable.makeWrapper ]; }
          ''
            mkdir -p "$out"/bin
            install -D --mode=0755 ${lib.escapeShellArg pkgs.stable.gitit}/bin/* --target-directory="$out/bin"
            wrapProgram "$out/bin/gitit" \
              --prefix PATH : ${lib.escapeShellArg (lib.makeBinPath [ pkgs.stable.git ])}
          '';
      config = {
        address = "127.0.0.1";
        port = 17503;
        wiki-title = "Monty's Wiki";
        repository-type = "Git";
        default-page-type = "Markdown";
        base-url = "https://wiki.swallow-chickadee.ts.net";
        authentication-method = "generic";
        front-page = "Front Page";
        no-delete = lib.strings.concatStringsSep ", " [
          "Front Page"
          "Help"
        ];
      };
      autopush = {
        enable = true;
        remotePath = config.sops.secrets."gitit/remote".path;
        deployKeyPath = config.sops.secrets."gitit/deploy-key".path;
      };
    };

    tscaddy = {
      enable = true;
      nodes.wiki = {
        tailnetName = "swallow-chickadee";
        target = "http://127.0.0.1:${intToString config.services.gitit.config.port}";
        authKeyFile = config.sops.secrets."caddy/ts-authkey-gitit".path;
        dependencies = [ "gitit.service" ];
        extraProxyConfig = ''
          header_up REMOTE_USER {http.auth.user.tailscale_login}
        '';
      };
    };

    caddy.virtualHosts."http://wiki.cule.gay, https://wiki.cule.gay".extraConfig = ''
      @http protocol http
      redir @http https://{host}{uri} 308
      import tailscale_handle_wiki
    '';
  };

  sops.secrets = {
    "gitit/remote" = {
      sopsFile = ../secrets/gitit/autopush.yaml;
      format = "yaml";
      key = "remoteURL";
      mode = "0400";
      owner = config.services.gitit.user;
      group = config.services.gitit.group;
    };
    "gitit/deploy-key" = {
      sopsFile = ../secrets/gitit/autopush.yaml;
      format = "yaml";
      key = "deployKey";
      mode = "0400";
      owner = config.services.gitit.user;
      group = config.services.gitit.group;
    };
    "caddy/ts-authkey-gitit" = {
      sopsFile = ../secrets/caddy/ts-authkey-gitit.txt;
      format = "binary";
      mode = "0440";
      owner = config.services.caddy.user;
      group = config.services.caddy.group;
    };
  };
}
