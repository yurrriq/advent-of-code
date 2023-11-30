{
  description = "My Haskell solutions to Advent of Code problems";

  inputs = {
    emacs-overlay = {
      inputs = {
        nixpkgs.follows = "nixpkgs";
        nixpkgs-stable.follows = "nixpkgs-stable";
      };
      url = "github:nix-community/emacs-overlay";
    };
    flake-parts.url = "github:hercules-ci/flake-parts";
    nixpkgs.url = "github:nixos/nixpkgs/release-22.11";
    nixpkgs-stable.follows = "nixpkgs";
    pre-commit-hooks-nix = {
      inputs = {
        nixpkgs.follows = "nixpkgs";
        nixpkgs-stable.follows = "nixpkgs-stable";
      };
      url = "github:cachix/pre-commit-hooks.nix";
    };
    treefmt-nix = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:numtide/treefmt-nix";
    };
  };

  outputs = inputs@{ flake-parts, self, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        inputs.pre-commit-hooks-nix.flakeModule
        inputs.treefmt-nix.flakeModule
      ];

      systems = [
        "x86_64-linux"
      ];

      flake = {
        overlays = {
          default = _final: prev: rec {
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
        };
      };

      perSystem = { config, pkgs, self', system, ... }: {
        _module.args.pkgs = import inputs.nixpkgs {
          overlays = [
            inputs.emacs-overlay.overlay
            self.overlays.default
          ];
          inherit system;
        };

        devShells.default = pkgs.mkShell {
          FONTCONFIG_FILE = pkgs.makeFontsConf {
            fontDirectories = [
              (pkgs.nerdfonts.override { fonts = [ "Iosevka" ]; })
            ];
          };

          inputsFrom = [
            self'.packages.advent-of-code.env
            config.pre-commit.devShell
          ];

          nativeBuildInputs = with pkgs; [
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
            rnix-lsp
            which
            xelatex-noweb
          ];
        };

        packages = {
          advent-of-code = pkgs.haskellPackages.callCabal2nix
            "advent-of-code"
            (pkgs.nix-gitignore.gitignoreSource [ ] ./.)
            { };

          default = self'.packages.advent-of-code;
        };

        pre-commit.settings.hooks = {
          treefmt.enable = true;
        };

        treefmt = {
          projectRootFile = ./flake.nix;
          programs = {
            deadnix.enable = true;
            hlint.enable = true;
            nixpkgs-fmt.enable = true;
            ormolu = {
              enable = true;
              ghcOpts = [
                "TemplateHaskell"
              ];
            };
            prettier.enable = true;
            shellcheck.enable = true;
          };
          settings.formatter = {
            prettier = {
              excludes = [
                "input/2022/day05.txt"
              ];
            };
          };
        };
      };
    };
}
