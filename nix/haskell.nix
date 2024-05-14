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
          free-algebras = hprev.callCabal2nix "free-algebras"
            (prev.fetchFromGitHub {
              owner = "yurrriq";
              repo = "free-algebras";
              rev = "f3d130b9ce3d4f53ab8a11b1149c8590233cbdb1";
              hash = "sha256-C3Fi2dEyrQvqfk5XMzl1vpoeH7lxkQPxveqhOtwcoYc=";
            })
            { };
        };
      };
    };
  };

  perSystem = { pkgs, self', ... }: {
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
    };

    packages = {
      advent-of-code = pkgs.haskellPackages.callCabal2nix
        "advent-of-code"
        (pkgs.nix-gitignore.gitignoreSource [ ] ../.)
        { };

      default = self'.packages.advent-of-code;
    };

    treefmt = {
      programs = {
        hlint.enable = true;
        ormolu = {
          enable = true;
          ghcOpts = [
            "LambdaCase"
            "TemplateHaskell"
          ];
        };
      };
    };
  };
}
