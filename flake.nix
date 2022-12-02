{
  description = "My Haskell solutions to Advent of Code problems";

  inputs = {
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs/release-22.11";
  };

  outputs = { self, emacs-overlay, flake-utils, nixpkgs }: {
    overlay = final: prev: rec {
      haskellPackages = prev.haskellPackages.override {
        overrides = hfinal: hprev: {
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

      myEmacs = prev.emacsWithPackagesFromUsePackage {
        alwaysEnsure = true;
        config = ./emacs.el;
      };

      noweb = prev.noweb.override {
        icon-lang = prev.icon-lang.override {
          withGraphics = false;
        };
      };

      xelatex-noweb = prev.texlive.combine {
        inherit (prev.texlive) scheme-small;
        inherit noweb;
        # Packages for Noweb
        inherit (prev.texlive)
          braket
          catchfile
          datatool
          datetime
          # dirtytalk
          fmtcount
          framed
          fvextra
          # glossaries
          # glossaries-extra
          hardwrap
          ifplatform
          latexmk
          mfirstuc
          minted
          substr
          titlesec
          # tkz-base
          todonotes
          tufte-latex
          xetex
          xindy
          xfor
          xstring
          ;
        # Packages for GAP
        inherit (prev.texlive)
          helvetic
          enumitem
          ;
      };
    };
  } // flake-utils.lib.eachSystem [ "x86_64-linux" ] (
    system:
    let
      pkgs = import nixpkgs {
        overlays = [
          emacs-overlay.overlay
          self.overlay
        ];
        inherit system;
      };
    in
    {
      defaultPackage =
        pkgs.haskellPackages.callCabal2nix
          "advent-of-code"
          (pkgs.nix-gitignore.gitignoreSource [ ] ./.)
          { };

      devShell = pkgs.mkShell {
        FONTCONFIG_FILE = pkgs.makeFontsConf {
          fontDirectories = [
            (pkgs.nerdfonts.override { fonts = [ "Iosevka" ]; })
          ];
        };

        buildInputs = with pkgs; [
          cabal-install
          gap-full
          ghc
          ghcid
          gitAndTools.pre-commit
          gnumake
          haskell-language-server
          haskellPackages.hlint
          haskellPackages.ormolu
          haskellPackages.pointfree
          (
            idrisPackages.with-packages
              (
                with idrisPackages; [
                  contrib
                  effects
                  lightyear
                ]
              )
          )
          myEmacs
          nixpkgs-fmt
          noweb
          picat
          python3
          python3Packages.pygments
          self.defaultPackage.${system}.env.nativeBuildInputs
          which
          xelatex-noweb
        ];
      };

      packages.advent-of-code = self.defaultPackage.${system};
    }
  );
}
