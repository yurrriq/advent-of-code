IDRIS ?= idris
OPTS  ?= -p contrib -p effects -p lightyear -i src

DAYS := 01 02 03 04

.PHONY: shell

all: $(addprefix output/,$(foreach day,$(DAYS),day$(day).txt))

bin/day%: src/Data/Advent/Day%.idr input/day%.txt
	@mkdir -p bin
	$(IDRIS) $(OPTS) -o $@ $<

output/day%.txt: bin/day%
	@mkdir -p output
	$< >$@

clean:
	find . -name '*.ibc' -delete
	rm -fr bin/ output/

shell:
	nix-shell -p haskellPackages.idris gcc gmp libffi
