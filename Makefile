idris ?= idris
opts  ?= -X ElabReflection -p contrib -p effects -p lightyear -i src
parts ?= one two

days := 01 02 03 04 06 07 08 09

.PHONY: shell day05

day05:
	stack build
	stack exec day05 > output/day05.txt

all: $(addprefix output/,$(foreach day,${days},day${day}.txt)) day05

bin/day%: src/Data/Advent/Day%.lidr input/day%.txt
	@ mkdir -p bin
	${idris} ${opts} -o $@ $<

output/day%.txt: bin/day%
	@ mkdir -p output
	$< ${parts} >$@

clean:
	find . -name '*.ibc' -delete
	rm -fr bin/ output/

shell:
	nix-shell -p haskellPackages.idris gcc gmp libffi
