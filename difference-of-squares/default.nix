{ nixpkgs ? import <nixpkgs> {} }:

with nixpkgs;

let

  gems = callPackage ../. {};

in

stdenv.mkDerivation rec {
  name = "difference_of_squares-${version}";
  version = "0.0.1";

  buildInputs = [ gems ruby ];
}
