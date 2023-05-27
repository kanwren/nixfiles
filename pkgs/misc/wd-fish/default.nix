{ lib, buildFishPlugin, gnused, wd-fish-src }:

buildFishPlugin {
  pname = "wd-fish";
  version = "unstable-2023-05-27";

  src = wd-fish-src;

  preInstall = ''
    sed -e "s;sed;${gnused}/bin/sed;g" -i functions/*
  '';

  meta = with lib; {
    description = "Directory navigation plugin for oh-my-fish";
    license = licenses.mit;
    platforms = platforms.unix;
  };
}
