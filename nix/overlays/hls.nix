self: super:

let
  hlsVersion = "0.6.0";

  hls-ghc-executable = { ghcVersion, sha256 }: {
    execName = "haskell-language-server-${ghcVersion}";
    url = "https://github.com/haskell/haskell-language-server/releases/download/${hlsVersion}/haskell-language-server-Linux-${ghcVersion}.gz";
    inherit sha256;
  };

  hls-wrapper = {
    execName = "haskell-language-server-wrapper";
    url = "https://github.com/haskell/haskell-language-server/releases/download/${hlsVersion}/haskell-language-server-wrapper-Linux.gz";
    sha256 = "064anwhrb61mj8d7y0a2irmns1hc37pb53yppsdwdql538i7v17v";
  };
  hls-executables = [ hls-wrapper ] ++ builtins.map hls-ghc-executable [
    { ghcVersion = "8.6.4"; sha256 = "0wa7kqqk1ca2ii6lcpv77k9lnx6lsjv33bjr3z6afxln0fc360p1"; }
    { ghcVersion = "8.6.5"; sha256 = "1qvwbriv3194iyiyjc23kas2xx05igwi2r2vcyj12qx9cq19kslj"; }
    { ghcVersion = "8.8.2"; sha256 = "13sn94lf5vyd41wj6r5pyxr87jliy7rgyw76ia3f37zh2kl1yy3m"; }
    { ghcVersion = "8.8.3"; sha256 = "13l7b7wf6lfy0q91lbh22p559myq5xcmmxqmky1rjav8vr829587"; }
    { ghcVersion = "8.8.4"; sha256 = "02rr4ayq45vvy52l4qjilbvfrr4nsdzg3q1agc83qffs1aw2nc5i"; }
    { ghcVersion = "8.10.1"; sha256 = "0fi340xs70rwf2w6j2frif1walgr1wcmm0qdy4d1y36vcv5nhs1k"; }
    { ghcVersion = "8.10.2"; sha256 = "009gcywzn6ah2mcxmvd92kr8yncmikgi2y92kbc38pdlddni1aqs"; }
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
