{ stdenv
, fetchFromGitHub
}:

stdenv.mkDerivation {
  name = "kak-mirror";
  src = fetchFromGitHub {
    owner = "Delapouite";
    repo = "kakoune-mirror";
    rev = "5710635f440bcca914d55ff2ec1bfcba9efe0f15";
    sha256 = "0fd65clx9p6sslrl3l25m8l9ihl2mqrvvmmkjqr3bgk16vip3jds";
  };
  installPhase = ''
    mkdir -p "$out"/share/kak/autoload/plugins/mirror
    cp -r mirror.kak "$out"/share/kak/autoload/plugins/mirror
  '';
}

