# compiler := ghc862
compiler := default


.PHONY: build shell


build: default.nix
	nix-$@ --argstr compiler ${compiler}


shell:
	nix-$@ --argstr compiler ${compiler}


default.nix: package.yaml
	cabal2nix --maintainer yurrriq --shell . >$@
