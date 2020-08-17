{ pkgs, std }:

args@{ piazza_id, piazza_email, piazza_password, slack_token, channel, bot_name }:

let
  python = pkgs.python37;
  python-env = with pkgs; python.withPackages (p: with p; [
    (python.pkgs.buildPythonPackage rec {
      pname = "piazza-api";
      version = "0.11.0";
      src = fetchPypi {
        inherit pname version;
        sha256 = "17w2ma77a86riz36fkgvkpq3rrym4ic09l7ni8ky2bjd61zgz0fr";
      };
      propagatedBuildInputs = [ six requests ];
    })
    (python.pkgs.buildPythonPackage rec {
      pname = "slacker";
      version = "0.14.0";
      src = fetchPypi {
        inherit pname version;
        sha256 = "022ixpjfs566r8880qzwbdk9bxshy52h722w5ch4w4aw0abwbrqs";
      };
      propagatedBuildInputs = [ requests ];
      doCheck = false;
    })
  ]);
in pkgs.stdenv.mkDerivation {
  name = "piazza-slackbot";

  src = pkgs.fetchFromGitHub {
    owner = "t-davidson";
    repo = "piazza-slackbot";
    rev = "ae075c0e56756ebc64ec70df3aedbabbe6632067";
    sha256 = "0swyx0rf2nzpyvcqby1wpx1m94dn67g9b6cicqwpizvmmmazrvb5";
  };

  buildInputs = [ pkgs.makeWrapper ];
  buildPhase = ''
    mkdir -p "$out/bin"
    cp "$src"/slackbot.py "$out/bin/.piazza-slackbot-wrapped"

    sed -e ' ${
      std.string.intercalate ";" (std.list.for (std.set.toList args) (kv:
        ''s/^${kv._0}[[:space:]]*=[[:space:]]*""/${kv._0} = "${kv._1}"/''
      ))
    } ' -i "$out/bin/.piazza-slackbot-wrapped"
  '';
  installPhase = ''
    makeWrapper \
      "${python-env}/bin/python" \
      "$out"/bin/piazza-slackbot \
      --add-flags "$out/bin/.piazza-slackbot-wrapped"
  '';
}

