{ stdenv
, fetchFromGitHub
, lib
}:

stdenv.mkDerivation {
  name = "kakoune-themes";
  src = fetchFromGitHub {
    owner = "anhsirk0";
    repo = "kakoune-themes";
    rev = "4e80b493ac58d96d55bc0650c96d1c782c1f3f04";
    sha256 = "sha256-9T9EoYgDq8xxXd5ihpPJPoy5EvigwmGAmyPKHzfdmyI=";
  };
  installPhase = ''
    target="$out"/share/kak
    mkdir -p "$target"
    cp -r colors "$target"
  '';
  meta = with lib; {
    description = "Custom config, syntax-highlighting, color schemes for kakoune";
    homepage = "https://github.com/anhsirk0/kakoune-themes";
    licenses = licenses.gpl3;
  };
}
