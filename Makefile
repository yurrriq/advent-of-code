PREFIX ?= .
cpif   ?= | cpif

day_srcs = $(foreach srcdir,day $(2) $(3) $(4),src/${srcdir}/$(1).nw)

NW_SRCS := \
$(call day_srcs,01, gap) \
$(call day_srcs,02, haskell) \
$(call day_srcs,04, haskell)


GAP_SRCS := $(patsubst src/gap/%.nw,src/Day%.g,$(filter src/gap/%.nw,${NW_SRCS}))

HS_SRCS := $(patsubst src/haskell/%.nw,src/Data/AOC19/Day%.hs,$(filter src/haskell/%.nw,${NW_SRCS}))

SRCS := ${GAP_SRCS} ${HS_SRCS}

PDF := docs/aoc.pdf

ifneq (,$(findstring B,$(MAKEFLAGS)))
latexmk_flags = -gg
endif

latexmk_flags += -cd -pdf


.PHONY: all
all: ${SRCS} ${PDF}


.PHONY: build
build: release.nix default.nix $(wildcard src/**.hs) $(wildcard app/**.hs)
	@ nix build -f $<


default.nix: package.yaml VERSION
	@ cabal2nix . --maintainer yurrriq --hpack >$@


escape_underscores := 'sed "/^@use /s/_/\\\\_/g;/^@defn /s/_/\\\\_/g"'
src/tex/allcode.tex: ${NW_SRCS}
	noweave -filter ${escape_underscores} -n -index $^ ${cpif} $@


.INTERMEDIATE: $(patsubst docs/%,src/%,${PDF})
docs/%.pdf: src/tex/%.pdf
	@ mkdir -p $(@D)
	@ cp $< $@

src/tex/%.pdf: src/tex/%.tex src/tex/allcode.tex
	latexmk $(latexmk_flags) $<


src/Day%.g: src/gap/%.nw
	@ mkdir -p $(@D)
	notangle -R'$(@F)' $< ${cpif} $@


src/Data/AOC19/Day%.hs: src/haskell/%.nw
	@ mkdir -p $(@D)
	notangle -R'$(@F)' $< ${cpif} $@
