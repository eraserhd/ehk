#!/bin/sh

pdflatex moreso || exit $?
rm -f index.tex || exit $?
csi -s index.sch || exit $?
pdflatex moreso || exit $?
pdflatex moreso || exit $?
open moreso.pdf
