{ buildGoModule }:
buildGoModule {
  pname = "ts-l4-rproxy";
  version = "0.0.1";
  src = ./.;

  runVend = true;
  vendorHash = "sha256-FG5sDOfkHMoLkCelT5LzLLgkfJYsYxfDKP6DwUWSLlY=";

  env.CGO_ENABLED = 0;

  ldflags = [
    "-s"
    "-w"
  ];

  meta = {
    mainProgram = "ts-l4-rproxy";
  };
}
