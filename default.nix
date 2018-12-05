{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, hashable, hpack, stdenv
      , trifecta, unordered-containers
      }:
      mkDerivation {
        pname = "advent-of-code";
        version = "0.2.1.2";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          base bytestring hashable trifecta unordered-containers
        ];
        libraryToolDepends = [ hpack ];
        executableHaskellDepends = [ base bytestring ];
        preConfigure = "hpack";
        homepage = "https://github.com/yurrriq/aoc18#readme";
        description = "Advent of Code 2018";
        license = stdenv.lib.licenses.bsd3;
        maintainers = with stdenv.lib.maintainers; [ yurrriq ];
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
