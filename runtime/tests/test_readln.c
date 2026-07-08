#include "coal/runtime.h"
#include "coal/io.h"
#include "coal/string.h"
#include "coal/value_api.h"
#include <stdio.h>
#include <string.h>
#include <assert.h>

/*
 * Automated test for rt_readln() using temporary file
 */

int
main(void)
{
    rt_runtime_init();

    printf("Running readln tests...\n");

    // Create a temporary file with test input
    FILE *tmp = tmpfile();
    assert(tmp != NULL);

    fprintf(tmp, "Hello, World!\n");
    fprintf(tmp, "Line with UTF-8: 😀\n");
    fprintf(tmp, "\n"); // Empty line
    fprintf(tmp, "Last line");
    rewind(tmp);

    // Save original stdin
    FILE *orig_stdin = stdin;

    // Redirect stdin to our test file
    stdin = tmp;

    // Test 1: Read normal line
    char *line1 = rt_readln();
    assert(strcmp(line1, "Hello, World!") == 0);
    printf("test_readln_normal: PASS\n");

    // Test 2: Read UTF-8 line
    char *line2 = rt_readln();
    assert(strcmp(line2, "Line with UTF-8: 😀") == 0);
    printf("test_readln_utf8: PASS\n");

    // Test 3: Read empty line
    char *line3 = rt_readln();
    assert(strlen(line3) == 0);
    assert(strcmp(line3, "") == 0);
    printf("test_readln_empty: PASS\n");

    // Test 4: Read line without newline
    char *line4 = rt_readln();
    assert(strcmp(line4, "Last line") == 0);
    printf("test_readln_no_newline: PASS\n");

    // Test 5: Test wrapper
    rewind(tmp);
    rt_value_t line_v = coal_readln();
    rt_string_t *line5 = rt_string_unbox(line_v);
    assert(strcmp(rt_string_data(line5), "Hello, World!") == 0);
    printf("test_readln_v_wrapper: PASS\n");

    // Restore stdin
    stdin = orig_stdin;
    fclose(tmp);

    printf("All readln tests passed!\n");

    return 0;
}
