
all: moreso.pdf

moreso.ss: moreso.w
	cheztangle moreso.w

moreso.tex: moreso.w
	chezweave moreso.w

moreso.pdf: moreso.tex
	pdftex -shell-escape moreso.tex
