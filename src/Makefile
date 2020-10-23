BIN_DIR ?= ../bin

LATEXMKFLAGS := -Werror -pdfxe -shell-escape

NW_SRCS := $(wildcard *.nw)
DEFS_SRCS := $(patsubst %.nw,%.defs,${NW_SRCS})
IR_SRCS := $(patsubst %.nw,%.ir,${NW_SRCS})
TEX_SRCS := $(patsubst %.nw,%.tex,${NW_SRCS})
HS_SRCS := $(patsubst %.nw,%.hs,${NW_SRCS})

ifneq (,$(findstring B,${MAKEFLAGS}))
CPIF := >
LATEXMKFLAGS += -gg
.SECONDARY: ${TEX_SRCS}
else
CPIF ?= | cpif
.INTERMEDIATE: ${TEX_SRCS}
endif

.INTERMEDIATE: ${DEFS_SRCS} ${IR_SRCS}

.PHONY: all

all: book.pdf ${HS_SRCS}

book.pdf: book.tex preamble.tex ${TEX_SRCS}
	@ latexmk ${LATEXMKFLAGS} $<

%.defs: %.nw
	@ nodefs $< Util.nw | sort -u ${CPIF} $@

%.tex: %.nw %.defs
	@ noweave \
		-delay \
		-indexfrom $*.defs \
		-filter '$(addprefix ${BIN_DIR}/,prefix-labels) $*' \
		-filter 'tee $*.ir' \
		-latex $< ${CPIF} $@
	@ noindex book

%.hs: %.nw
	@ notangle -R$*.hs $< ${CPIF} $@

.PHONY: clean clean-minted clobber watch

clean: clean-minted
	@ latexmk ${LATEXMKFLAGS} -c

clean-minted:
	@ $(RM) minted*

clobber: clean-minted
	@ latexmk ${LATEXMKFLAGS} -C

watch: clean-minted
	@ latexmk ${LATEXMKFLAGS} -new-viewer- -pvc -silent -time -use-make book
