#undef STANDALONE
#define STANDALONE 0
#include "scheme.c"
#include <assert.h>

/* An allocator that allows us to check on things */

typedef struct test_allocation_tag {
	struct test_allocation_tag *next;
	size_t size;
	int freed;
	char memory[1];
} test_allocation_t;

test_allocation_t *allocations = 0;

void *test_malloc(size_t size)
{
	test_allocation_t *a = (test_allocation_t *)malloc(sizeof (test_allocation_t) + size);
	a->size = size;
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

void verify_all_memory_freed()
{
	test_allocation_t *a;
	int all_freed = 1;
	char line[100];
	int i, line_ofs;

	for (a = allocations; a; a = a->next) {
		if (a->freed)
			continue;

		all_freed = 0;
		printf("-- %p (%d bytes) was not freed:\n", &a->memory, (int)a->size);

		line_ofs = 0;
		while (line_ofs < a->size) {
			memset(line, ' ', sizeof(line));
			sprintf(line, "%04x ", line_ofs);
			for (i = line_ofs; i < a->size && i < line_ofs + 16; ++i) {
				sprintf(line + 5 + 3*(i - line_ofs), "%02x ", ((unsigned char*)&a->memory)[i]);

				line[5 + 3*16 + (i - line_ofs) + 1] = (a->memory[i] < ' ' || a->memory[i] >= 127) ? '.' : a->memory[i];
			}
			*strchr(line,0) = ' ';
			line[5 + 3*16 + 17] = 0;
			puts(line);

			line_ofs += 16;
		}
		printf("\n");
	}

	assert(all_freed);
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

void run(void (* check) (scheme *))
{
	scheme sc;
	memset(&sc, 0xfd, sizeof(sc));
	scheme_init_custom_alloc(&sc, test_malloc, test_free);
	scheme_set_output_port_file(&sc, stdout);

	check(&sc);

	scheme_deinit(&sc);
	verify_all_memory_freed();
	reset_test_allocator();
}

void check_simplest_case(scheme *sc)
{
	// do nothing
}

void check_with_init_scm(scheme *sc)
{
	FILE *initf = fopen("init.scm", "r");
	scheme_load_named_file(sc, initf, "init.scm");
	fclose(initf);
}

int main(int argc, char *argv[])
{
	run(check_simplest_case);
	run(check_with_init_scm);
	exit(0);
}
