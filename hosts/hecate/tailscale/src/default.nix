{ lib, buildGoModule }:

buildGoModule rec {
  pname = "caddy-with-extensions";
  version = "unstable-2025-03-07";
  src = ./.;

  runVend = true;
  vendorHash = "sha256-D6L5mFchyWz48zrdS/VwzmtycGAMyI+gkdHk/sWiJYg=";

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
