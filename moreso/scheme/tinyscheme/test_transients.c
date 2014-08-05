#undef STANDALONE
#define STANDALONE 0
#include "scheme.c"
#include <assert.h>

void check_transients_is_initialized_to_null(scheme *sc)
{
	assert(!sc->transients);
}

void check_transients_are_recorded(scheme *sc)
{
	char *ptrs[3];
	int i, j, count;
	struct transient *t;
	int found;

	for (i = 0; i < 3; ++i)
		ptrs[i] = allocate_transient(sc, 5);

	count = 0;
	for (t = sc->transients; t != NULL; t = t->next)
		++count;
	assert(count == 3);

	for (i = 0; i < 3; ++i) {
		found = 0;
		for (t = sc->transients; t != NULL; t = t->next)
			if (ptrs[i] == (char*) &t->memory)
				found = 1;
		assert(found);
	}
}

void check_free_transients_clears_list(scheme *sc)
{
	int i;

	sc = scheme_init_new();
	for (i = 0; i < 3; ++i)
		allocate_transient(sc, 5);

	free_transients(sc);
	assert(!sc->transients);
}

void run(void (* check) (scheme *))
{
	scheme sc;
	memset(&sc, 0xfd, sizeof(sc));
	scheme_init(&sc);

	check(&sc);

	scheme_deinit(&sc);
}

int main(int argc, char **argv)
{
	run(check_transients_is_initialized_to_null);
	run(check_transients_are_recorded);
	run(check_free_transients_clears_list);
	exit(0);
}
