#include "coal/runtime.h"
#include "coal/io.h"
#include "coal/string.h"
#include <stdio.h>
#include <string.h>

/*
 * Interactive test for rt_readln()
 *
 * This test is not run automatically because it requires user input.
 * To run manually:
 *   make build/tests/test_readln_interactive
 *   ./build/tests/test_readln_interactive
 */

int
main(void)
{
    rt_runtime_init();

    printf("Interactive readln test\n");
    printf("=======================\n\n");

    printf("Enter a line of text (or Ctrl+D to exit): ");
    fflush(stdout);

    char *input = rt_readln();

    printf("\nYou entered: ");
    rt_print_string(input);
    printf("\n");

    printf("Length (bytes): %zu\n", strlen(input));

    // Test with UTF-8
    printf("\nEnter text with emoji: ");
    fflush(stdout);

    char *input2 = rt_readln();

    printf("You entered: ");
    rt_print_string(input2);
    printf("\n");

    // Test empty input
    printf("\nPress Enter without typing anything: ");
    fflush(stdout);

    char *input3 = rt_readln();

    printf("Empty string length: %zu\n", strlen(input3));
    printf("Empty string is: \"%s\"\n", input3);

    printf("\nTest completed successfully!\n");

    return 0;
}
