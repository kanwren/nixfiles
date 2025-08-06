{
  lib,
  buildGoModule,
}:
buildGoModule rec {
  pname = "caddy-with-plugins";
  version = "unstable-2025-08-05";
  src = ./.;

  runVend = true;
  vendorHash = "sha256-g2pys2/gftZGHJ2Hy1mKOiZHFBHtDS7OJub4Tkhvlk4=";

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
