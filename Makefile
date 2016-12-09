IDRIS ?= idris
OPTS  ?= -p contrib -p effects -p lightyear -i src

DAYS := 01 02 03 04 06

.PHONY: shell day05

day05:
	stack build
	stack exec day05 > output/day05.txt

all: $(addprefix output/,$(foreach day,$(DAYS),day$(day).txt)) day05

bin/day%: src/Data/Advent/Day%.idr input/day%.txt
	@ mkdir -p bin
	$(IDRIS) $(OPTS) -o $@ $<

output/day%.txt: bin/day%
	@ mkdir -p output
	$< >$@

clean:
	find . -name '*.ibc' -delete
	rm -fr bin/ output/

shell:
	nix-shell -p haskellPackages.idris gcc gmp libffi
