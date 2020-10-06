self: super:

let
  hlsVersion = "0.5.0";

  hls-ghc-executable = { ghcVersion, sha256 }: {
    execName = "haskell-language-server-${ghcVersion}";
    url = "https://github.com/haskell/haskell-language-server/releases/download/${hlsVersion}/haskell-language-server-Linux-${ghcVersion}.gz";
    inherit sha256;
  };

  hls-wrapper = {
    execName = "haskell-language-server-wrapper";
    url = "https://github.com/haskell/haskell-language-server/releases/download/${hlsVersion}/haskell-language-server-wrapper-Linux.gz";
    sha256 = "1m1zn3lar2lgsr29vxbcay58mpny1gzzf1mfmpyda6rr61gy5z2a";
  };
  hls-executables = [ hls-wrapper ] ++ builtins.map hls-ghc-executable [
    { ghcVersion = "8.6.4"; sha256 = "1n24lpq230r3q6661lllmk4jm5fi9z47g15p2dzm3vznahsy1qxh"; }
    { ghcVersion = "8.6.5"; sha256 = "0bd9d7pm61l9lr81jbw6x5crb2v0pdl1pzj3fand8sibz2sxnnab"; }
    { ghcVersion = "8.8.2"; sha256 = "1w3sfrb0nbfxrjr7a63pf8ddx58nvm3qsgqkswfkklyjyxhw70g6"; }
    { ghcVersion = "8.8.3"; sha256 = "00qfvay2fr4vmiraxv1f40g7cns2iqlav5sh454pv91z6k95pg1i"; }
    { ghcVersion = "8.8.4"; sha256 = "167bqvlxvhscbfdj4p29l026n8235b8zq1ycjsjyx5in19kz74an"; }
    { ghcVersion = "8.10.1"; sha256 = "0868ffnkcq7ls7cnwlxj2hx05yj67lgsc5hkdlvn5am1z49idy5i"; }
    { ghcVersion = "8.10.2"; sha256 = "09al67kj3yrig682j986pfn4pvvbjddxj2aqj6vgv8d67sjirn4j"; }
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
