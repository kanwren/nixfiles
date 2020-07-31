self: super:

let
  hlsVersion = "0.2.2";

  hls-ghc-executable = { ghcVersion, sha256 }: {
    execName = "haskell-language-server-${ghcVersion}";
    url = "https://github.com/haskell/haskell-language-server/releases/download/${hlsVersion}/haskell-language-server-Linux-${ghcVersion}.gz";
    inherit sha256;
  };

  hls-wrapper = {
    execName = "haskell-language-server-wrapper";
    url = "https://github.com/haskell/haskell-language-server/releases/download/${hlsVersion}/haskell-language-server-wrapper-Linux.gz";
    sha256 = "00zc7jqrc4r69r2v35dgjam5r0x5s0l0p3jacm9g01xcjqp19vb3";
  };
  hls-executables = [ hls-wrapper ] ++ builtins.map hls-ghc-executable [
    { ghcVersion = "8.6.4";  sha256 = "1gd103yc3g568jqiihkxds44s9ggj7m40mg92sm3s0sqbgxp1wqp"; }
    { ghcVersion = "8.6.5";  sha256 = "10znxc057j4914n2lpir3byxclzqv3r014rzgijf2nc7zrd8f6ym"; }
    { ghcVersion = "8.8.2";  sha256 = "0ax102l82dpinbnk6sx06v5rapz3lb393iaz61wwff7dpsads9dm"; }
    { ghcVersion = "8.8.3";  sha256 = "0gin9qx9bnch7644cfzh5vv0k5fskga55xfwgwln64z786z48dnp"; }
    { ghcVersion = "8.8.4";  sha256 = "0mfy74q5lcpjm7chhvj32mz2ps3ylry1h9hq72f6i34qp4amjxh6"; }
    { ghcVersion = "8.10.1"; sha256 = "0cbykwn4hrxs9asnz8x4ydbfmfpyhf8j47plk7lwz0sac7x2y3cs"; }
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
