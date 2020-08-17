self: super:

let
  hlsVersion = "0.3.0";

  hls-ghc-executable = { ghcVersion, sha256 }: {
    execName = "haskell-language-server-${ghcVersion}";
    url = "https://github.com/haskell/haskell-language-server/releases/download/${hlsVersion}/haskell-language-server-Linux-${ghcVersion}.gz";
    inherit sha256;
  };

  hls-wrapper = {
    execName = "haskell-language-server-wrapper";
    url = "https://github.com/haskell/haskell-language-server/releases/download/${hlsVersion}/haskell-language-server-wrapper-Linux.gz";
    sha256 = "1fi9ahimckhgbnw3szyvbxnjis1lpszl3dxyxav064nxi0pi9y6m";
  };
  hls-executables = [ hls-wrapper ] ++ builtins.map hls-ghc-executable [
    { ghcVersion = "8.6.4";  sha256 = "1q3vq2gdb3zsmxab1fmq4s7f7p4r3kdzn709aqf83hhfx18sn1rj"; }
    { ghcVersion = "8.6.5";  sha256 = "0cwfl77w04c6ixi7gshn8n0vi2v5klw3rsp9qnwrh6qk0zrj5wbd"; }
    { ghcVersion = "8.8.2";  sha256 = "061k9vzryskfbfyxv5lp3ci0bif4k609ajk0cmyvxdyrhkhnfyjn"; }
    { ghcVersion = "8.8.3";  sha256 = "0dk47bdaf4qg8ax55abasvaghlczhhh3mqa368n23pysy8rvlp4m"; }
    { ghcVersion = "8.8.4";  sha256 = "1d4lwb8jkwcrzd472n3k16s51qchqg2rq2gw324mzf5z9xsq7plk"; }
    { ghcVersion = "8.10.1"; sha256 = "04vqak02fnm01pqaqr49b18ckay0g3sdlkb468giyyjphflgjg8h"; }
  ];

  fetchHls = executables:
    super.runCommand "haskell-language-server-${hlsVersion}" {} ''
      mkdir -p "$out"/bin
      ${super.lib.concatMapStringsSep
        "\n"
        ({ execName, url, sha256 }: ''gunzip -c ${builtins.fetchurl { inherit url sha256; }} > "$out"/bin/${execName}'')
        executables
      }
      chmod +x "$out"/bin/*
    '';
in {
  haskell-language-server = fetchHls hls-executables;
}
