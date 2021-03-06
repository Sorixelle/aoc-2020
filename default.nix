{ pkgs ? import <nixpkgs> { } }:

pkgs.stdenv.mkDerivation {
  name = "aoc-2020";
  version = "1";

  nativeBuildInputs = with pkgs; [ (haskellPackages.ghcWithPackages (p: [ p.matrix ])) ];

  src = ./.;

  buildPhase = ''
    cd $src
    mkdir -p $out/tmp
    ghc -o $out/01ReportRepair -outputdir $out/tmp/1 01ReportRepair
    ghc -o $out/02PasswordPhilosophy -outputdir $out/tmp/2 02PasswordPhilosophy
    ghc -o $out/03TobogganTrajectory -outputdir $out/tmp/3 03TobogganTrajectory
    ghc -o $out/04PassportProcessing -outputdir $out/tmp/4 04PassportProcessing
    ghc -o $out/05BinaryBoarding -outputdir $out/tmp/5 05BinaryBoarding
    ghc -o $out/06CustomCustoms -outputdir $out/tmp/6 06CustomCustoms
    ghc -o $out/07HandyHaversacks -outputdir $out/tmp/7 07HandyHaversacks
    ghc -o $out/08HandheldHalting -outputdir $out/tmp/8 08HandheldHalting
    ghc -o $out/09EncodingError -outputdir $out/tmp/9 09EncodingError
    ghc -o $out/10AdapterArray -outputdir $out/tmp/10 10AdapterArray
    ghc -o $out/11SeatingSystem -outputdir $out/tmp/11 11SeatingSystem
    ghc -o $out/12RainRisk -outputdir $out/tmp/12 12RainRisk
  '';

  installPhase = ''
    rm -r $out/tmp
  '';
}
