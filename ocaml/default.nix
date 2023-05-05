let

  fetchTarballFromGitHub =
    { owner, repo, rev, sha256, ... }:
    builtins.fetchTarball {
      url = "https://github.com/${owner}/${repo}/tarball/${rev}";
      inherit sha256;
    };

  fromJSONFile = f: builtins.fromJSON (builtins.readFile f);

in

with import (fetchTarballFromGitHub (fromJSONFile ./nixpkgs-src.json)) { };

stdenv.mkDerivation {
  name = "exercism-ocaml-env";
  buildInputs = with pkgs; ([
    exercism
    pkgconfig
  ] ++ (with ocamlPackages; [
    base
    core
    dune
    findlib
    ocaml
    ounit
    utop
  ]));
}
