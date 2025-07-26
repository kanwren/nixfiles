{
  lib,
  fetchFromGitHub,
  buildFishPlugin,
  gnused,
}:
buildFishPlugin {
  pname = "wd-fish";
  version = "unstable-2024-06-24";

  src = fetchFromGitHub {
    owner = "fischerling";
    repo = "plugin-wd";
    rev = "ac46fb36724fa17d97965f9829ae29c61ec1e109";
    hash = "sha256-pRTEU9T+WkFxTdeZR1EZgW9FCeBsb3VbUvwFD27ZkZ4=";
  };

  preInstall = ''
    sed -e "s;sed;${gnused}/bin/sed;g" -i functions/*
  '';

  meta = with lib; {
    description = "Directory navigation plugin for oh-my-fish";
    license = licenses.mit;
    platforms = platforms.unix;
  };
}
