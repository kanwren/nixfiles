{ lib
, buildGoModule
,
}:
buildGoModule rec {
  pname = "caddy-with-extensions";
  version = "unstable-2025-05-10";
  src = ./.;

  runVend = true;
  vendorHash = "sha256-jrY0NpI27mcTGZ+qTw7fyRsHllpCsnESk1J0l+0riro=";

  ldflags = [
    "-s"
    "-w"
    "-X github.com/caddyserver/caddy/v2.CustomVersion=${version}"
  ];

  tags = [
    "nobadger"
    "nomysql"
    "nopgx"
  ];

  meta = {
    homepage = "https://github.com/tailscale/caddy-tailscale";
    license = lib.licenses.asl20;
    mainProgram = "caddy";
  };
}
