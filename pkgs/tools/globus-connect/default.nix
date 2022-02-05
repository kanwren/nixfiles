{ tcl-8_6
, tk-8_6
, tcllib
, stdenv
, lib
, fetchurl
, makeWrapper
, patchelf
, gcc-unwrapped
, glibc
, python3
, libaudit
, makeDesktopItem
, createDesktop ? true
}:

let
  pname = "globus-connect";
  version = "3.1.5";

  desktopItem = makeDesktopItem {
    name = pname;
    exec = "globusconnectpersonal -gui";
    desktopName = "Globus Connect Personal";
    genericName = "File Transfer Tool";
  };
in

let
  tcl-8_6_10 = tcl-8_6.overrideAttrs (_: rec {
    release = "8.6";
    version = "${release}.10";

    src = fetchurl {
      url = "mirror://sourceforge/tcl/tcl${version}-src.tar.gz";
      sha256 = "sha256-UZbb9mOOPfjVyHtYFcjCt1hJbrbw5BRGWWyaTmONh+0=";
    };
  });
  tk-8_6_10 = tk-8_6.override (_: { tcl = tcl-8_6_10; });

  drv = stdenv.mkDerivation {
    inherit pname version;

    src = builtins.fetchTarball {
      url = "https://downloads.globus.org/globus-connect-personal/linux/stable/globusconnectpersonal-latest.tgz";
      sha256 = "sha256-fh5j0H8ycpOPXSAjuTUYcvrN1UTbsUaE0jRc6qy1iOk=";
    };

    nativeBuildInputs = [ makeWrapper patchelf ];

    installPhase = ''
      mkdir -p "$out"/bin "$out"/share/globus-connect-personal
      cp -r * "$out"/share/globus-connect-personal
      makeWrapper "$out"/share/globus-connect-personal/globusconnect \
        "$out"/bin/globusconnect \
        --prefix PATH : "${lib.makeBinPath [ python3 tcl-8_6_10 ]}" \
        --set TCL_LIBRARY "${tcl-8_6_10}/lib/tcl8.6" \
        --prefix TCLLIBPATH " " "${tk-8_6_10}/lib/tk8.6 ${tcllib}/lib/tcllib1.20"
      makeWrapper "$out"/share/globus-connect-personal/globusconnectpersonal \
        "$out"/bin/globusconnectpersonal \
        --prefix PATH : "${lib.makeBinPath [ python3 tcl-8_6_10 ]}" \
        --set TCL_LIBRARY "${tcl-8_6_10}/lib/tcl8.6" \
        --prefix TCLLIBPATH " " "${tk-8_6_10}/lib/tk8.6 ${tcllib}/lib/tcllib1.20"
      ln -s "$out"/share/globus-connect-personal/util/remove_globusconnect \
        "$out"/bin/remove_globusconnect
    '' + lib.optionalString createDesktop ''
      mkdir -p "$out"/share/applications
      cp ${desktopItem}/share/applications/* "$out"/share/applications
    '';

    preFixup = ''
      LD_LIBRARY_PATH="${
        lib.makeLibraryPath [
          "$out/share/globus-connect-personal/gt_amd64"
          gcc-unwrapped.lib
          libaudit.out
        ]
      }''${LD_LIBRARY_PATH:+:''${LD_LIBRARY_PATH}}"

      # Fix libs
      for lib in $(find "$out"/share/globus-connect-personal/gt_amd64/lib -type f)
      do
        patchelf --set-rpath "$LD_LIBRARY_PATH:$(patchelf --print-rpath "$lib")" "$lib"
      done

      # Fix bins
      for bin in \
        "$out"/share/globus-connect-personal/tclkit \
        "$out"/share/globus-connect-personal/gt_amd64/bin/* \
        "$out"/share/globus-connect-personal/gt_amd64/sbin/*
      do
        patchelf --set-rpath "$LD_LIBRARY_PATH:$(patchelf --print-rpath "$bin")" "$bin"
        patchelf --set-interpreter "${glibc.out}/lib/ld-linux-x86-64.so.2" "$bin"
      done
    '';

    meta = {
      homepage = "https://www.globus.org/";
      description = "Globus lets you share data on your storage systems with collaborators at other institutions.";
      license = lib.licenses.asl20;
    };
  };
in
drv // lib.optionalAttrs createDesktop { inherit desktopItem; }

