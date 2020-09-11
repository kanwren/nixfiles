self: super:

let
  hlsVersion = "0.4.0";

  hls-ghc-executable = { ghcVersion, sha256 }: {
    execName = "haskell-language-server-${ghcVersion}";
    url = "https://github.com/haskell/haskell-language-server/releases/download/${hlsVersion}/haskell-language-server-Linux-${ghcVersion}.gz";
    inherit sha256;
  };

  hls-wrapper = {
    execName = "haskell-language-server-wrapper";
    url = "https://github.com/haskell/haskell-language-server/releases/download/${hlsVersion}/haskell-language-server-wrapper-Linux.gz";
    sha256 = "026hmd6c8ilf7p5h3rk63ywd358hm0xbmniplnkdfilgri3j26sm";
  };
  hls-executables = [ hls-wrapper ] ++ builtins.map hls-ghc-executable [
    { ghcVersion = "8.6.4"; sha256 = "0fhzm9190mgya08mcjz9zrd34p1s3k1a1l9dzxpxm7ij9b43n333"; }
    { ghcVersion = "8.6.5"; sha256 = "0d4lwfy3ywrmz5qppzq11khk9n9744zrmgp5nl618dcl5di1w0aa"; }
    { ghcVersion = "8.8.2"; sha256 = "1q68gvz9pjzfwsssa6w2qqxx7x6lxqnlvn8wm89g2z64ga5ayay2"; }
    { ghcVersion = "8.8.3"; sha256 = "1ii7ad3kbl27nfyrvwqb5ban09vdap2y56gyd8gbs3g6zjrphx81"; }
    { ghcVersion = "8.8.4"; sha256 = "0df1l8clrch19dsz8dxr2y7qzdm6c71jwik28l5sl5p1qzngiv47"; }
    { ghcVersion = "8.10.1"; sha256 = "0ldsxq02mzxhp11690ajqn0yfw3m115057bgxza9336ahdfxni7m"; }
    { ghcVersion = "8.10.2"; sha256 = "14dzs78s8rns04rhw0g87c4jkvhnfindz8gkj7aw2ygd4hw9ayyk"; }
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
