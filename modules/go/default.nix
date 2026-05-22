{
  flake.modules.homeManager.go =
    { pkgs, config, ... }:
    {
      home = {
        packages = [
          pkgs.delve
        ];

        sessionPath = [ "${config.programs.go.env.GOPATH}/bin" ];
      };

      programs.go = {
        enable = true;
        package = pkgs.go_latest;
      };

      programs.fish.shellAbbrs =
        let
          commandAbbr = cmd: {
            position = "command";
            expansion = cmd;
          };
        in
        {
          "gob" = commandAbbr "go build";
          "gog" = commandAbbr "go get";
          "goi" = commandAbbr "go install";
          "gom" = commandAbbr "go mod";
          "gomi" = commandAbbr "go mod init";
          "gomt" = commandAbbr "go mod tidy";
          "gor" = commandAbbr "go run";
          "got" = commandAbbr "go test";
        };
    };
}
