.PHONY: build
build: release.nix default.nix $(wildcard src/**.hs) $(wildcard app/**.hs)
	@ nix build -f $<


default.nix: package.yaml VERSION
	@ cabal2nix . --maintainer yurrriq --hpack >$@
