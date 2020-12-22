PREFIX ?= .
cpif   ?= | cpif

day_srcs = $(foreach srcdir,day $(2) $(3) $(4),_src/${srcdir}/$(1).nw)

NW_SRCS := \
$(call day_srcs,01, gap) \
$(call day_srcs,01, haskell/2018) \
$(call day_srcs,02, haskell/2018) \
$(call day_srcs,04, haskell/2019) \
$(call day_srcs,08, haskell/2019)


GAP_SRCS := $(patsubst _src/gap/%.nw,src/Day%.g,$(filter _src/gap/%.nw,${NW_SRCS}))

HS_SRCS := \
$(patsubst _src/haskell/2018/%.nw,src/AdventOfCode/Year2018/Day%.hs,$(filter _src/haskell/2018/%.nw,${NW_SRCS})) \
$(patsubst _src/haskell/2019/%.nw,src/AdventOfCode/Year2019/Day%.hs,$(filter _src/haskell/2019/%.nw,${NW_SRCS}))


SRCS := ${GAP_SRCS} ${HS_SRCS}

PDF := docs/aoc.pdf

ifneq (,$(findstring B,$(MAKEFLAGS)))
latexmk_flags = -gg
endif

latexmk_flags += -cd -pdf


.PHONY: all
all: ${SRCS} ${PDF}


.PHONY: build
build: default.nix $(wildcard src/**.hs) $(wildcard app/**.hs)
	@ nix build -f $<


default.nix: package.yaml VERSION
	@ cabal2nix . --maintainer yurrriq --hpack >$@


escape_underscores := 'sed "/^@use /s/_/\\\\_/g;/^@defn /s/_/\\\\_/g"'
_src/tex/allcode.tex: ${NW_SRCS}
	noweave -filter ${escape_underscores} -n -index $^ ${cpif} $@


.INTERMEDIATE: $(patsubst docs/%,_src/%,${PDF})
docs/%.pdf: _src/tex/%.pdf
	@ mkdir -p $(@D)
	@ cp $< $@

_src/tex/%.pdf: _src/tex/%.tex _src/tex/allcode.tex
	latexmk $(latexmk_flags) $<


src/Day%.g: _src/gap/%.nw
	@ mkdir -p $(@D)
	notangle -R'$(@F)' $< ${cpif} $@


src/AdventOfCode/Year2018/Day%.hs: _src/haskell/2018/%.nw
	@ mkdir -p $(@D)
	notangle -R'$(@F)' $< ${cpif} $@


src/AdventOfCode/Year2019/Day%.hs: _src/haskell/2019/%.nw
	@ mkdir -p $(@D)
	notangle -R'$(@F)' $< ${cpif} $@


src/AdventOfCode/Util.hs: _src/haskell/Util.nw
	@ mkdir -p $(@D)
	notangle -R'$(@F)' $< ${cpif} $@


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




idris ?= idris
opts  ?= -X ElabReflection -p contrib -p effects -p lightyear -i src
parts ?= one two

idr_days  := 02 03 04 06 07 08 10
lidr_days := 01 09

.PHONY: shell day05

day05:
	stack build
	stack exec day05 > output/day05.txt

all: $(addprefix output/,$(foreach day,${days},day${day}.txt)) day05

bin/day%: src/Data/Advent/Day%.lidr input/day%.txt
	@ mkdir -p bin
	${idris} ${opts} -o $@ $<

bin/day%: src/Data/Advent/Day%.idr input/day%.txt
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
