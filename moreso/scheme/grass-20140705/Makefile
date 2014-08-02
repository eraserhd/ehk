### Makefile for GRASS


PREFIX = $(HOME)
PLATFORM = linux

DEFAULT_IMPLEMENTATION = chicken
BOOTSTRAP_MODULES = lib/syntax lib/match lib/misc read alexpand \
	program pp lib/normalize dtype write eval top env
CHICKEN_VERSION = 4.8.0.3
CHICKEN_DOWNLOAD_ADDRESS = http://code.call-cc.org/releases/4.8.0/chicken-$(CHICKEN_VERSION).tar.gz

CC = gcc
CSI = csi
CSC = csc
CSCFLAGS = -k -S -O3 -d0

S48 = scheme48
S48LIBPATH = /usr/lib/scheme48-1.8
S48HEAPSIZE = 10000000
S48VM = $(S48LIBPATH)/scheme48vm
S48VMFLAGS = -h $(S48HEAPSIZE)

GSC = gsc
GSCFLAGS = 

RACO = raco
RACOFLAGS =

SCHEME = $(DEFAULT_IMPLEMENTATION)-grass

TOPLEVEL_TARGETS = all clean spotless check check-chicken check-scheme48 check-gambit \
	check-racket dist install bootstrap distfiles help


.PHONY: $(TOPLEVEL_TARGETS)


all: $(SCHEME) build/grass-grass.scm


bootstrap: 
	sh tools/bootstrap.sh $(CC) -o chicken-grass

clean:
	rm -f *-grass.scm scheme48-grass.image chicken-grass build/*.o

spotless: clean
	rm -f build/core.scm scheme48-grass

dist: bootstrap/chicken-grass.c bootstrap/runtime.c build/core.scm
	sh tools/dist.sh

distfiles: bootstrap/chicken-grass.c bootstrap/runtime.c build/core.scm

bootstrap/chicken-grass.c: chicken-grass
	mkdir -p bootstrap
	cp chicken-grass.c bootstrap

bootstrap/runtime.c:
	mkdir -p bootstrap
	if test \! -d chicken-$(CHICKEN_VERSION); then \
	  wget $(CHICKEN_DOWNLOAD_ADDRRESS); \
	  tar xzf chicken-$(CHICKEN_VERSION).tar.gz; \
	fi
	cd chicken-$(CHICKEN_VERSION); \
	cp chicken.h library.c runtime.c build-version.c buildtag.h ../bootstrap

build/core.scm: chicken/base.scm tools/bootstrap.scm $(BOOTSTRAP_MODULES:%=%.scm)
	$(CSI) -bnq chicken/base.scm tools/bootstrap.scm

build/grass-grass.scm: grass/base.scm build/core.scm grass/main.scm
	cat $^ >$@

chicken-grass: chicken/base.scm build/core.scm chicken/main.scm build/numbers.o
	$(CSC) $(CSCFLAGS) chicken/main.scm -o $@ -x build/numbers.o

build/numbers.o: chicken/numbers.scm chicken/numbers-c.c chicken/numbers-c.h
	$(CSC) -c $(CSCFLAGS) $< -o $@ -x -unit numbers -J -Ichicken

scheme48-grass: scheme48-grass.image
	echo '#!/bin/sh' >$@
	echo 'exec $(S48VM) $(S48VMFLAGS) -i $(PREFIX)/.scheme/scheme48-grass.image -a "$$@"' >>$@
	chmod +x $@

scheme48-grass.image: scheme48/base.scm build/core.scm scheme48/main.scm \
	scheme48/packages.scm
	(echo ',config ,load "scheme48/packages.scm"'; \
	echo ',open grass-main load-dynamic-externals'; \
	echo '(set-library-path "$(S48LIBPATH)")'; \
	echo ',build run-main "scheme48-grass.image" no-warnings'; \
	echo ',exit') | $(S48)

gambit-grass: gambit/base.scm build/core.scm \
	gambit/main.scm 
	$(GSC) $(GSCFLAGS) -o $@ -exe gambit/main.scm

build/racket-grass.scm: racket/prefix.scm racket/base.scm build/core.scm \
	racket/main.scm
	echo "#lang r5rs" >$@
	cat $^ >>$@

racket-grass: build/racket-grass.scm
	$(RACO) exe $(RACOFLAGS) -o $@ $<

check: all build/grass-grass.scm
	@cd tests; \
	../$(SCHEME) fac.scm; \
	../$(SCHEME) '(case-sensitive #f) (load "r4rstest.scm")'; \
	../$(SCHEME) r5rs_pitfalls.scm; \
	../$(SCHEME) compiler.scm; \
	../$(SCHEME) '(begin (load "../lib/ec.scm") (load "ec-tests.scm"))'; \
	../$(SCHEME) ../build/grass-grass.scm '(case-sensitive #f) (load "r4rstest.scm")'; \
	../$(SCHEME) ../build/grass-grass.scm r5rs_pitfalls.scm; \
	../$(SCHEME) '(begin (load "../lib/megalet.scm") (load "test-megalet.scm"))'; \
	echo; \
	echo === $(SCHEME) executing grass ===; \
	time ../$(SCHEME) ../build/grass-grass.scm fac.scm; \
	echo; \
	echo === $(SCHEME) executing grass executing grass ===; \
	time ../$(SCHEME) ../build/grass-grass.scm \
	  ../build/grass-grass.scm fac.scm

check-chicken:	
	$(MAKE) DEFAULT_IMPLEMENTATION=chicken all check
check-scheme48:
	$(MAKE) DEFAULT_IMPLEMENTATION=scheme48 all check
check-gambit:
	$(MAKE) DEFAULT_IMPLEMENTATION=gambit all check
check-racket:
	$(MAKE) DEFAULT_IMPLEMENTATION=racket all check

install: all
	mkdir -p $(PREFIX)/bin
	ln -sf $$PWD/$(SCHEME) $(PREFIX)/bin/scheme

help:
	@echo
	@echo "Toplevel targets:"
	@echo
	@for x in $(TOPLEVEL_TARGETS); do \
	  echo "  $$x"; \
	done
	@echo

#XXX
#
# - add this to misc/make and just run it when invoked (also rename to "GNUMakefile")
