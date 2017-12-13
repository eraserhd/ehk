
all: moreso.pdf

moreso.ss: moreso.w
	cheztangle moreso.w

moreso.tex: moreso.w
	chezweave moreso.w

moreso.pdf: moreso.tex
	pdflatex -shell-escape moreso.tex
