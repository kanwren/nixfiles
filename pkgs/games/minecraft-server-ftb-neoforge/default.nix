{
  modpackInfo,
  modpackVersion,
  stdenv,
  lib,
  runtimeShell,
  fetchurl,
  runCommandNoCC,
  curl,
  cacert,
  udev,
  gnused,
  parallel,
  jq,
  openjdk,
}:
assert builtins.all (name: builtins.hasAttr name modpackInfo) ["name" "id"];
assert builtins.all (name: builtins.hasAttr name modpackVersion) ["version" "versionId" "configHash" "neoforgeVersion" "neoforgeHash"]; let
  server-config = stdenv.mkDerivation {
    pname = "minecraft-server-${modpackInfo.name}-config";
    inherit (modpackVersion) version;
    dontUnpack = true;
    nativeBuildInputs = [curl cacert jq parallel];
    buildPhase = ''
      mkdir -p "$out"
      curl --fail --silent --show-error --location 'https://api.feed-the-beast.com/v1/modpacks/modpack/${modpackInfo.id}/${modpackVersion.versionId}' \
      | jq --raw-output --arg out "$out" '
        .files[]
        | select(.clientonly | not)
        | "\(.url)\t\($out)/\(.path)/\(.name)"
      ' \
      | parallel --will-cite --colsep '\t' 'curl --fail --silent --show-error --location --write-out '\'''%{response_code} %{url}\n'\''' {1} --create-dirs --output {2}'
    '';
    outputHash = modpackVersion.configHash;
    outputHashMode = "recursive";
    preferLocalBuild = true;
  };

  server-libraries = stdenv.mkDerivation {
    pname = "minecraft-server-${modpackInfo.name}-libraries";
    inherit (modpackVersion) version;
    src = fetchurl {
      url = "https://maven.neoforged.net/releases/net/neoforged/neoforge/${modpackVersion.neoforgeVersion}/neoforge-${modpackVersion.neoforgeVersion}-installer.jar";
      hash = "sha256-EWEPeI/125Tfelcrs9it2RM2mhLMjyDvGKfyvXBCRiM=";
    };
    nativeBuildInputs = [openjdk];
    dontUnpack = true;
    buildPhase = ''
      java -jar "$src" --install-server "$out"
    '';
    fixupPhase = ''
      rm -f "$out"/{'run.bat','run.sh','user_jvm_args.txt'}
      cp $out/libraries/net/neoforged/neoforge/${modpackVersion.neoforgeVersion}/unix_args.txt $out/args.txt
    '';
    outputHash = modpackVersion.neoforgeHash;
    outputHashMode = "recursive";
  };

  server-launcher = let
    escape = lib.strings.escapeShellArg;
  in
    runCommandNoCC "minecraft-server" {nativeBuildInputs = [gnused];} ''
      mkdir -p "$out"/bin
      printf '#! %s\n' ${escape runtimeShell} > "$out"/bin/minecraft-server

      ${lib.optionalString stdenv.hostPlatform.isLinux ''
        printf '
        export LD_LIBRARY_PATH=${escape (lib.makeLibraryPath [udev])}"''${LD_LIBRARY_PATH:+:"''${LD_LIBRARY_PATH}"}"
        ' >> "$out"/bin/minecraft-server
      ''}

      printf '
      echo '\'''Applying initial configuration from '\'''${escape server-config}
      cp --recursive --no-clobber --preserve ${escape server-config}/. .
      chmod -R u+w .
      ' >> "$out"/bin/minecraft-server

      printf '%s' ${escape (lib.getExe' openjdk "java")} >> "$out"/bin/minecraft-server
      sed 's;libraries;${server-libraries}/libraries;g' ${escape "${server-libraries}/args.txt"} \
      | while IFS= read -r line || [ -n "$line" ]; do
        printf ' %s' "$line" >> "$out"/bin/minecraft-server
      done
      printf ' %s' '"$@"' >> "$out"/bin/minecraft-server

      chmod +x "$out"/bin/minecraft-server
    '';
in
  server-launcher
