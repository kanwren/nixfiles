{ stdenv
, fetchFromGitHub
}:

stdenv.mkDerivation {
  name = "kak-mirror";
  src = fetchFromGitHub {
    owner = "Delapouite";
    repo = "kakoune-mirror";
    rev = "5710635f440bcca914d55ff2ec1bfcba9efe0f15";
    sha256 = "sha256-uslx4zZhvjUylrPWvTOugsKYKKpF0EEz1drc1Ckrpjk=";
  };
  installPhase = ''
    mkdir -p "$out"/share/kak/autoload/plugins/mirror
    cp -r mirror.kak "$out"/share/kak/autoload/plugins/mirror
  '';
}

