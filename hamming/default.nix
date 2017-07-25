{ nixpkgs ? import <nixpkgs> {} }:

with nixpkgs;

let

  gems = callPackage ../. {};

in

stdenv.mkDerivation rec {
  name = "hamming-${version}";
  version = "0.0.1";

  buildInputs = [ gems ruby ];
}
