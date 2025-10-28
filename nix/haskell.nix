{ ... }:

{
  flake = {
    overlays.haskell = _final: prev: {
      haskellPackages = prev.haskellPackages.override {
        overrides = _hfinal: hprev: {
          digits = hprev.callCabal2nix "digits"
            (prev.fetchFromGitHub {
              owner = "yurrriq";
              repo = "digits";
              rev = "c3a2c2bacc4a2e2c51beefa2fdb90da9a5bddf6b";
              hash = "sha256-/n2gf33zShj6LexHRplp975teCZyLAsg0rmXK9AHoK0=";
            })
            { };
        };
      };
    };
  };

  perSystem = { lib, pkgs, self', ... }: {
    devShells.haskell = pkgs.mkShell {
      inputsFrom = [
        self'.packages.advent-of-code.env
      ];

      nativeBuildInputs = with pkgs; [
        cabal-install
        ghc
        ghcid
        haskell-language-server
        haskellPackages.ormolu
        haskellPackages.pointfree
      ];

      shellHook = ''
        export LD_LIBRARY_PATH="${lib.makeLibraryPath [ pkgs.zlib ] }"
      '';
    };

    packages = {
      advent-of-code =
        let
          src = lib.fileset.toSource {
            fileset = lib.fileset.unions [
              ../VERSION
              ../input
              ../package.yaml
              ../src
              ../test
            ];
            root = ../.;
          };
          pkg = pkgs.haskellPackages.callCabal2nix "advent-of-code" src.outPath { };
        in
        pkgs.haskell.lib.overrideCabal pkg {
          haddockFlags = [
            "--html-location='https://hackage.haskell.org/package/$pkgid/docs/'"
            "--hyperlink-source"
            "--quickjump"
          ];
          librarySystemDepends = [
            pkgs.zlib
          ];
        };

      default = self'.packages.advent-of-code;
    };

    treefmt = {
      programs = {
        hlint.enable = true;
        ormolu = {
          enable = true;
          ghcOpts = [
            "GHC2021"
            "TemplateHaskell"
          ];
        };
      };
    };
  };
}
