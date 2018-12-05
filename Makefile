# compiler := ghc862
compiler := default


.PHONY: build ghci shell


build: default.nix
	nix-$@ --argstr compiler ${compiler}


ghci: default.nix
	nix-shell --argstr compiler ${compiler} --command ghci


shell: default.nix
	nix-$@ --argstr compiler ${compiler}


default.nix: package.yaml
	cabal2nix --maintainer yurrriq --shell . >$@
