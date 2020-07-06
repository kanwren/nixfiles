let
  fetchGithubArchive = { owner, repo, rev, sha256 }: fetchTarball {
    url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
    inherit sha256;
  };
in import (fetchGithubArchive {
  owner = "nix-community";
  repo = "NUR";
  rev = "b272b690c921e6dff689a3717c8d277d8b6af981";
  sha256 = "0z4ajs43fa6zxw5w3kjl8b7p7wzl0gzb6x72d3m6f3r28bkqdswq";
})
