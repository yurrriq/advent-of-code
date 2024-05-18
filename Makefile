PREFIX ?= ${CURDIR}/docs
cpif   ?= | cpif

day_srcs = \
_src/day/$(1)/$(2).nw \
$(wildcard _src/gap/$(1)/$(2).nw) \
$(wildcard _src/haskell/$(1)/$(2).nw)

years := \
2015 2018 2019 2021

days := \
01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25

year_srcs = \
$(foreach day,${days},\
$(if $(wildcard _src/$1/day/${day}.nw),\
_src/$1/day/${day}.nw \
$(wildcard _src/$1/gap/${day}.nw) \
$(wildcard _src/$1/haskell/${day}.nw),))

NW_SRCS := $(foreach year,${years},$(call year_srcs,${year}))

# FIXME
GAP_SRCS := $(patsubst _src/gap/%.nw,src/Day%.g,$(filter _src/gap/%.nw,${NW_SRCS}))

HS_SRCS := \
$(patsubst _src/2015/haskell/%.nw,src/AdventOfCode/Year2015/Day%.hs,$(filter _src/2015/haskell/%.nw,${NW_SRCS})) \
$(patsubst _src/2018/haskell/%.nw,src/AdventOfCode/Year2018/Day%.hs,$(filter _src/2018/haskell/%.nw,${NW_SRCS})) \
$(patsubst _src/2019/haskell/%.nw,src/AdventOfCode/Year2019/Day%.hs,$(filter _src/2019/haskell/%.nw,${NW_SRCS})) \
$(patsubst _src/2021/haskell/%.nw,src/AdventOfCode/Year2021/Day%.hs,$(filter _src/2021/haskell/%.nw,${NW_SRCS}))

SRCS := ${GAP_SRCS} ${HS_SRCS}

PDF := docs/aoc.pdf
# TODO
# PDF := ${PREFIX}/aoc.pdf

ifneq (,$(findstring B,$(MAKEFLAGS)))
latexmk_flags = -gg
endif

latexmk_flags += -cd -pdf


.PHONY: all
all: ${SRCS} ${PDF}


.PHONY: build
build: flake.nix $(wildcard src/**.hs) $(wildcard app/**.hs)
	@ nix build

escape_underscores := 'sed "/^@use /s/_/\\\\_/g;/^@defn /s/_/\\\\_/g"'

_src/tex/2015.tex: $(call year_srcs,2015)
	noweave -filter ${escape_underscores} -n -index $^ ${cpif} $@

_src/tex/2018.tex: $(call year_srcs,2018)
	noweave -filter ${escape_underscores} -n -index $^ ${cpif} $@

_src/tex/2019.tex: $(call year_srcs,2019)
	noweave -filter ${escape_underscores} -n -index $^ ${cpif} $@

_src/tex/2021.tex: $(call year_srcs,2021)
	noweave -filter ${escape_underscores} -n -index $^ ${cpif} $@


.INTERMEDIATE: $(patsubst docs/%,_src/%,${PDF})
docs/%.pdf: _src/tex/%.pdf
	@ mkdir -p $(@D)
	@ cp $< $@

_src/tex/%.pdf: _src/tex/%.tex $(foreach year,${years},_src/tex/${year}.tex)
	latexmk $(latexmk_flags) $<


src/Day%.g: _src/gap/%.nw
	@ mkdir -p $(@D)
	notangle -R'$(@F)' $< ${cpif} $@


src/AdventOfCode/Year2015/Day%.hs: _src/2015/haskell/%.nw
	@ mkdir -p $(@D)
	notangle -R'$(@F)' $< ${cpif} $@


src/AdventOfCode/Year2018/Day%.hs: _src/2018/haskell/%.nw
	@ mkdir -p $(@D)
	notangle -R'$(@F)' $< ${cpif} $@


src/AdventOfCode/Year2019/Day%.hs: _src/2019/haskell/%.nw
	@ mkdir -p $(@D)
	notangle -R'$(@F)' $< ${cpif} $@


src/AdventOfCode/Year2021/Day%.hs: _src/2021/haskell/%.nw
	@ mkdir -p $(@D)
	notangle -R'$(@F)' $< ${cpif} $@


src/AdventOfCode/Util.hs: _src/haskell/Util.nw
	@ mkdir -p $(@D)
	notangle -R'$(@F)' $< ${cpif} $@


idris ?= idris
opts  ?= -X ElabReflection -p contrib -p effects -p lightyear -i src
parts ?= one two

idr_days  := 02 03 04 06 07 08 10
lidr_days := 01 09

# FIXME
# all: $(addprefix output/,$(foreach day,${days},day${day}.txt))

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
	latexmk $(latexmk_flags) -C -f ${PDF}
	find . -name '*.ibc' -delete
	rm -fr output/
