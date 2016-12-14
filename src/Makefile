PANDOC ?= pandoc

LIDRFILES := Data/Advent/Day01.lidr

TEXFILES := $(LIDRFILES:.lidr=.tex)

all: all.pdf

all.pdf: all.tex
	xelatex -shell-escape $<
	xelatex -shell-escape $<

all.tex: advent.tex ${TEXFILES}
	${PANDOC} --filter pandoc-minted.py --latex-engine=xelatex \
	-f markdown+lhs+tex_math_single_backslash -t latex+lhs \
	--top-level-division=chapter -N \
	--toc \
	-o $@ $(foreach tex,$(filter-out $<,$^),-A $(tex)) \
	$<

%.tex: %.lidr
	${PANDOC} --filter pandoc-minted.py --latex-engine=xelatex \
	-f markdown+lhs+tex_math_single_backslash -t latex+lhs \
	--top-level-division=chapter -N \
	-o $@ $<
