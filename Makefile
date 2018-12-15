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

output/day%.txt: app/day%/Main.hs src/Day%.hs input/day%.txt
	$$(nix-build --argstr compiler ${compiler} --no-out-link)/bin/day$* >$@


output/day%.txt: app/day%/Main.hs src/Day%.lhs input/day%.txt
	$$(nix-build --argstr compiler ${compiler} --no-out-link)/bin/day$* >$@


src/%.md: src/%.lhs
	pandoc -f markdown+lhs -t gfm -o $@ $<
