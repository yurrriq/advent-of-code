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
    nixpkgs-stable.url = "github:nixos/nixpkgs/release-25.05";
    git-hooks-nix = {
      inputs.nixpkgs.follows = "nixpkgs";
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
        inputs.git-hooks-nix.flakeModule
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
              pkgs.nerd-fonts.iosevka
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
          convco.enable = true;
          treefmt.enable = true;
        };

        treefmt = {
          programs = {
            deadnix.enable = true;
            nixpkgs-fmt.enable = true;
            prettier.enable = true;
            shellcheck.enable = true;
            statix.enable = true;
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
