{ pkgs ? import ./nix {} }:

pkgs.haskellPackages.callPackage ./default.nix {}
