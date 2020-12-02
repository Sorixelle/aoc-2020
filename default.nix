{ pkgs ? import <nixpkgs> { } }:

pkgs.stdenv.mkDerivation {
  name = "aoc-2020";
  version = "1";

  nativeBuildInputs = with pkgs; [ ghc ];

  src = ./.;

  buildPhase = ''
    cd $src
    mkdir -p $out/tmp
    ghc -o $out/01ReportRepair -outputdir $out/tmp/1 01ReportRepair
    ghc -o $out/02PasswordPhilosophy -outputdir $out/tmp/2 02PasswordPhilosophy
  '';

  installPhase = ''
    rm -r $out/tmp
  '';
}
