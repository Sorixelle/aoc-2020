{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  name = "aoc-2020";

  buildInputs = with pkgs; [
    (haskellPackages.ghcWithPackages (p: [ p.matrix ]))
  ];
}
