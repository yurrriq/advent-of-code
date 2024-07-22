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
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    nixpkgs-stable.url = "github:nixos/nixpkgs/release-24.05";
    pre-commit-hooks-nix = {
      inputs = {
        nixpkgs.follows = "nixpkgs";
        nixpkgs-stable.follows = "nixpkgs-stable";
      };
      url = "github:cachix/git-hooks.nix";
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
        ./nix/emacs.nix
        ./nix/haskell.nix
        ./nix/noweb.nix
        ./nix/ocaml.nix
      ];

      systems = [
        "x86_64-linux"
      ];

      flake.overlays.default =
        let
          inherit (inputs.nixpkgs) lib;
        in
        lib.composeManyExtensions
          (lib.attrValues
            (lib.filterAttrs (name: _: name != "default") self.overlays));

      # FIXME: v2 works differently, I guess.
      flake.overlays.treefmt = _final: prev: {
        treefmt = prev.treefmt1;
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
            config.pre-commit.devShell
            self'.devShells.emacs
            self'.devShells.haskell
            self'.devShells.noweb
            self'.devShells.ocaml
          ];

          nativeBuildInputs = with pkgs; [
            gap-full
            gnumake
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
            picat
            python3
          ];
        };

        pre-commit.settings.hooks = {
          treefmt.enable = true;
        };

        treefmt = {
          projectRootFile = ./flake.nix;
          programs = {
            deadnix.enable = true;
            nixpkgs-fmt.enable = true;
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
