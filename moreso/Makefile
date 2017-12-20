
LIBDIRS	= .:./nanopass

all: moreso.pdf test

tests.ss moreso.ss: moreso.w
	cheztangle moreso.w

moreso.tex: moreso.w
	chezweave moreso.w

moreso.pdf: moreso.tex
	pdftex -shell-escape moreso.tex

test: tests.ss
	scheme --libdirs $(LIBDIRS) --program ./tests.ss

.PHONY: all test
