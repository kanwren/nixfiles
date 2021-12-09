{ mkTmuxPlugin
, fetchFromGitHub
}:

mkTmuxPlugin {
  pluginName = "nord-tmux";
  version = "unstable_2020-08-25";
  src = fetchFromGitHub {
    owner = "arcticicestudio";
    repo = "nord-tmux";
    rev = "5bb2086690e3e27378794fb68ed98e91f0cce059";
    sha256 = "0zlj834fpn8z7khdwjp8irzz2vvmc79qkp6jb7jk1xzlcxflc1qh";
  };
  rtpFilePath = "nord.tmux";
}

