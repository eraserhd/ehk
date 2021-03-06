#undef STANDALONE
#define STANDALONE 0
#include "scheme.c"
#include <assert.h>

int check_count = 0;
int fail_count = 0;
int error_count = 0;

void check(char *expression, char *expect)
{
	scheme *sc;
	char *statement;
	pointer result;
	FILE *initf;

	sc = scheme_init_new();
	initf = fopen("init.scm", "r");
	scheme_load_named_file(sc, initf, "init.scm");
	fclose(initf);
	scheme_set_input_port_file(sc, stdin);
	scheme_set_output_port_file(sc, stdout);

	statement = (char*)malloc(2*strlen(expression) +  2*strlen(expect) + 512);
	sprintf(statement, "\
(define (check)\n\
  (let ((result %s))\n\
    (if (equal? result '%s)\n\
      #t\n\
      (begin\n\
	(display \"FAIL: \")\n\
	(write '%s)\n\
	(display \" => \")\n\
	(write '%s)\n\
	(display \" got: \")\n\
	(write result)\n\
	(newline)\n\
	#f))))",
		expression, expect,
		expression,
		expect);
	scheme_load_string(sc, statement);
	free(statement);

	result = scheme_apply0(sc, "check");
	if (result == sc->T) {
		++check_count;
	} else if (result == sc->F) {
		++check_count;
		++fail_count;
	} else {
		++check_count;
		++error_count;
	}

	scheme_deinit(sc);
}

void check_string_p_returns_t_for_char_vectors()
{
	check("(string? '#(#\\a #\\b #\\c))", "#t");
	check("(string? '#())", "#t");
	check("(string? '#(#\\a #\\b 42))", "#f");
	check("(string? 42)", "#f");
}

void check_subvector_works()
{
	check("(subvector #(#\\a #\\b #\\c) 1 2)", "#(#\\b)");
	check("(subvector #(#\\a #\\b #\\c) 1)", "#(#\\b #\\c)");
	check("(subvector #(#\\a #\\b #\\c) 0)", "#(#\\a #\\b #\\c)");
	check("(subvector #(#\\a #\\b #\\c) 3)", "#()");
	check("(subvector #(1 2 3) 1 2)", "#(2)");
}

void check_vector_copy()
{
	check("(vector-append #(#\\a 2 #\\c))", "#(#\\a 2 #\\c)");
	check("(vector-append #(#\\a 3 #\\c) #(5 #f))", "#(#\\a 3 #\\c 5 #f)");
}

void check_vector_ref()
{
	check("(vector-ref \"abc\" 1)", "#\\b");
}

void check_vector_length()
{
	check("(vector-length \"abc\")", "3");
}

int main(int argc, char **argv)
{
	check_string_p_returns_t_for_char_vectors();
	check_subvector_works();
	check_vector_copy();
	check_vector_ref();
	check_vector_length();

	printf(" %d tests, %d failed, %d errors.\n", check_count, fail_count, error_count);
	if (fail_count == 0 && error_count == 0)
		exit(0);
	else
		exit(1);
}
