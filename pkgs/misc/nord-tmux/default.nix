{ mkTmuxPlugin
, fetchFromGitHub
}:

mkTmuxPlugin {
  pluginName = "nord-tmux";
  version = "unstable_2020-08-25";
  src = fetchFromGitHub {
    owner = "arcticicestudio";
    repo = "nord-tmux";
    rev = "4e2dc2a5065f5e8e67366700f803c733682e8f8c";
    sha256 = "0l97cqbnq31f769jak31ffb7bkf8rrg72w3vd0g3fjpq0717864a";
  };
  rtpFilePath = "nord.tmux";
}

