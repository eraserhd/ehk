
bin_PROGRAMS = bin/moreso

all: $(bin_PROGRAMS)

bin/%: src/%.scm
	gsc -exe -o $@ $<
