{ stdenv
, lib
, fetchFromGitHub
, symlinkJoin
  # deps
, python2
, python39Packages
, byobu
  # wallstreet
, apg
, atop
, bmon
, ccze
, cmatrix
, dnstop
, glances
, htop
, iotop
, iproute
, iptraf-ng
, jnettop
, jp2a
, latencytop
, logtop
, moreutils
, mplayer
, nload
, nmon
, powertop
, slurm
, tiptop
, tree
, unixtools
, vim
, vnstat
  # wallstreet
, altair
, ncurses
, newsboat
, rsstail
, ticker
, w3m
, wget
}:

let
  snetz = stdenv.mkDerivation {
    name = "snetz";
    src = fetchFromGitHub {
      owner = "rndc";
      repo = "snetz";
      rev = "d8f379d5ceb65eea0e573f7bb55afdda45183d18";
      sha256 = "0ks0jfi4f080m8ymva4rnybsg6c2yf3kksd837gvp4dcrnnz6c4h";
    };
    propagatedBuildInputs = [ python2 ];
    buildPhase = "true";
    installPhase = ''
      mkdir -p "$out"/bin
      # shebang will be patched
      cp -r "$src"/snetz.py "$out"/bin/snetz
      chmod +x "$out"/bin/snetz

      mkdir -p "$out"/share/man/man1
      cp "$src"/snetz.1.gz "$out"/share/man/man1
    '';

    meta = with lib; {
      homepage = "https://github.com/rndc/snetz";
      description = "Simple network bandwidth monitoring tool implemented in Python";
    };
  };

  deps = [
    byobu

    # unaccounted for:
    # speedometer
    # ethstatus
    # iptotal
    # itop
    # kerneltop
    # netmrg
    # ntop
    # sagan

    # TODO: the pygments program is scanning under /usr, won't find anything

    # hollywood
    apg
    atop
    bmon
    ccze
    cmatrix
    dnstop
    glances
    htop
    iotop
    iproute
    iptraf-ng
    jnettop
    jp2a
    latencytop
    logtop
    moreutils
    mplayer
    nload
    nmon
    powertop
    python39Packages.pygments
    slurm
    snetz
    tiptop
    tree
    unixtools.top
    vim
    vnstat

    # wallstreet
    altair
    wget
    ncurses
    newsboat
    rsstail
    ticker
    w3m
  ];

  hollywood-base = stdenv.mkDerivation {
    name = "hollywood";
    src = fetchFromGitHub {
      owner = "dustinkirkland";
      repo = "hollywood";
      rev = "35275a68c37bbc39d8b2b0e4664a0c2f5451e5f6";
      sha256 = "1kk5sjwq6hmfh83vl0i4kcs8cianmgkfm2m4cw1z4in4wpb2d8kx";
    };

    installPhase = ''
      mkdir -p "$out"
      cp -r * "$out"
    '';

    patches = [
      ./w3m.patch
      ./store.patch
    ];

    propagatedBuildInputs = deps;

    meta = with lib; {
      homepage = "https://github.com/dustinkirkland/hollywood";
      description = "Fills your console with Hollywood melodrama technobabble";
      license = licenses.asl20;
    };
  };
in

symlinkJoin {
  name = "hollywood";
  paths = [ hollywood-base ] ++ deps;
}
