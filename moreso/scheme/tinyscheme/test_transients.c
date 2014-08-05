#undef STANDALONE
#define STANDALONE 0
#include "scheme.c"
#include <assert.h>

void check_transients_is_initialized_to_null()
{
	scheme sc;
	memset(&sc,0xfd,sizeof(sc));

	scheme_init(&sc);
	assert(!sc.transients);

        scheme_deinit(&sc);
}

int main(int argc, char **argv)
{
	check_transients_is_initialized_to_null();
	exit(0);
}
