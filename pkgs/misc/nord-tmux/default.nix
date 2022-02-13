{ mkTmuxPlugin
, fetchFromGitHub
}:

mkTmuxPlugin {
  pluginName = "nord-tmux";
  version = "unstable_2020-08-25";
  src = fetchFromGitHub {
    owner = "arcticicestudio";
    repo = "nord-tmux";
    rev = "fb282780a13cf43a864d8f2b5f689e95551e2864";
    sha256 = "038pfg03rz601l2vwqda5nl8zfpjfihfb7qkslc6f9bd5kcppjzy";
  };
  rtpFilePath = "nord.tmux";
}

