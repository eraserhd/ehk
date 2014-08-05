#undef STANDALONE
#define STANDALONE 0
#include "scheme.c"
#include <assert.h>

/* An allocator that allows us to check on things */

typedef struct test_allocation_tag {
	struct test_allocation_tag *next;
	int freed;
	char memory[1];
} test_allocation_t;

test_allocation_t *allocations = 0;

void *test_malloc(size_t size)
{
	test_allocation_t *a = (test_allocation_t *)malloc(sizeof (test_allocation_t) + size);
	a->freed = 0;
	a->next = allocations;
	allocations = a;
	return &a->memory;
}

void test_free(void *ptr)
{
	test_allocation_t *a;

	for (a = allocations; a; a = a->next) {
		if ((void *)&a->memory == ptr) {
			assert(!a->freed);
			a->freed = 1;
			return;
		}
	}

	assert(0); // Was not allocated with test_malloc()
}

int pointer_was_freed(void *ptr)
{
	test_allocation_t *a;

	for (a = allocations; a; a = a->next)
		if ((void *)&a->memory == ptr)
			return a->freed;

	assert(0);
}

void reset_test_allocator()
{
	test_allocation_t *a, *next;

	a = allocations;
	while (a) {
		next = a->next;
		free(a);
		a = next;
	}

	allocations = 0;
}

/* Tests */

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

	for (i = 0; i < 3; ++i)
		allocate_transient(sc, 5);

	free_transients(sc);

	assert(!sc->transients);
}

void check_free_transients_frees_all_allocated_transients(scheme *sc)
{
	int i;
	void *ptrs[3];

	for (i = 0; i < 3; ++i)
		ptrs[i] = allocate_transient(sc, 5);

	free_transients(sc);

	for (i = 0; i < 3; ++i)
		assert(pointer_was_freed((char*)ptrs[i] - (int)&((struct transient *)0)->memory));
}

void run(void (* check) (scheme *))
{
	scheme sc;
	memset(&sc, 0xfd, sizeof(sc));
	scheme_init_custom_alloc(&sc, test_malloc, test_free);

	check(&sc);

	scheme_deinit(&sc);
	reset_test_allocator();
}

int main(int argc, char **argv)
{
	run(check_transients_is_initialized_to_null);
	run(check_transients_are_recorded);
	run(check_free_transients_clears_list);
	run(check_free_transients_frees_all_allocated_transients);
	exit(0);
}
