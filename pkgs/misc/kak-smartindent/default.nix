{ stdenv
, fetchFromGitHub
, lib
}:

stdenv.mkDerivation {
  name = "kak-smartindent";
  src = fetchFromGitHub {
    owner = "andreyorst";
    repo = "smarttab.kak";
    rev = "1dd3f33c4f65da5c13aee5d44b2e77399595830f";
    sha256 = "sha256-4kEXeYKJrEIzJ+aEP/zTUjtWcRbbKm5Levbe9w6ZiTw=";
  };
  installPhase = ''
    mkdir -p "$out"/share/kak/autoload/plugins/smartindent
    cp -r rc/*.kak "$out"/share/kak/autoload/plugins/smartindent
  '';
  meta = with lib; {
    description = "Automatic handling different styles of indentation and alignment";
    homepage = "https://github.com/andreyorst/smarttab.kak";
    licenses = licenses.mit;
  };
}

