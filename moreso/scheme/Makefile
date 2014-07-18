
bin_PROGRAMS = bin/moreso

test_SOURCES = $(wildcard test/*-test.scm)

all: $(bin_PROGRAMS) test

.PHONY: test
test: $(test_SOURCES)
	for t in $(test_SOURCES); do echo "$$t:"; gsi $$t || exit $$?; done

bin/%: src/%.scm
	gsc -exe -o $@ $<
