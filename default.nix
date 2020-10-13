{ pkgs ? import ./nix }:
let
  src = pkgs.nix-gitignore.gitignoreSource [] ./.;
in
pkgs.haskellPackages.callCabal2nix "aoc" src {}
