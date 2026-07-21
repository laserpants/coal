#include "coal/io.h"
#include "coal/string.h"
#include "coal/value_api.h"
#include "coal/runtime.h"
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <unistd.h>

/* Test reading an existing file */
static void
test_read_file_success(void)
{
    /* Create a temporary file */
    const char *filename = "test_temp_read.txt";
    const char *content = "Hello, World!\nThis is a test file.\n";

    FILE *f = fopen(filename, "w");
    assert(f != NULL);
    fputs(content, f);
    fclose(f);

    /* Read the file */
    rt_result_t *result = rt_read_file(filename);
    assert(result != NULL);
    assert(rt_result_status(result) == RT_READ_STATUS_OK);

    char *data = rt_result_value(result);
    assert(data != NULL);
    assert(strcmp(data, content) == 0);

    /* Clean up */
    unlink(filename);

    printf("test_read_file_success: PASS\n");
}

/* Test reading a non-existent file */
static void
test_read_file_not_found(void)
{
    const char *filename = "this_file_does_not_exist_xyz123.txt";

    rt_result_t *result = rt_read_file(filename);
    assert(result != NULL);
    assert(rt_result_status(result) == RT_READ_STATUS_FILE_NOT_FOUND);
    assert(rt_result_value(result) == NULL);

    printf("test_read_file_not_found: PASS\n");
}

/* Test reading an empty file */
static void
test_read_file_empty(void)
{
    const char *filename = "test_temp_empty.txt";

    /* Create empty file */
    FILE *f = fopen(filename, "w");
    assert(f != NULL);
    fclose(f);

    /* Read it */
    rt_result_t *result = rt_read_file(filename);
    assert(result != NULL);
    assert(rt_result_status(result) == RT_READ_STATUS_OK);

    char *data = rt_result_value(result);
    assert(data != NULL);
    assert(strlen(data) == 0);

    /* Clean up */
    unlink(filename);

    printf("test_read_file_empty: PASS\n");
}

/* Test reading a large file */
static void
test_read_file_large(void)
{
    const char *filename = "test_temp_large.txt";

    /* Create file with 10KB of data */
    FILE *f = fopen(filename, "w");
    assert(f != NULL);
    for (int i = 0; i < 1000; i++) {
        fprintf(f, "Line %d: The quick brown fox jumps over the lazy dog.\n",
                i);
    }
    fclose(f);

    /* Read it */
    rt_result_t *result = rt_read_file(filename);
    assert(result != NULL);
    assert(rt_result_status(result) == RT_READ_STATUS_OK);

    char *data = rt_result_value(result);
    assert(data != NULL);
    assert(strlen(data) > 10000); /* Should be over 10KB */

    /* Verify first and last lines */
    assert(strncmp(data, "Line 0:", 7) == 0);
    assert(strstr(data, "Line 999:") != NULL);

    /* Clean up */
    unlink(filename);

    printf("test_read_file_large: PASS\n");
}

/* Test reading a file with UTF-8 content */
static void
test_read_file_utf8(void)
{
    const char *filename = "test_temp_utf8.txt";
    const char *content = "Hello, 世界! 😀🌍\n";

    FILE *f = fopen(filename, "w");
    assert(f != NULL);
    fputs(content, f);
    fclose(f);

    rt_result_t *result = rt_read_file(filename);
    assert(result != NULL);
    assert(rt_result_status(result) == RT_READ_STATUS_OK);

    char *data = rt_result_value(result);
    assert(data != NULL);
    assert(strcmp(data, content) == 0);

    /* Clean up */
    unlink(filename);

    printf("test_read_file_utf8: PASS\n");
}

/* Test writing to a file */
static void
test_write_file_success(void)
{
    const char *filename = "test_temp_write.txt";
    const char *content = "Testing write functionality!\n";

    rt_result_t *result = rt_write_file(filename, content);
    assert(result != NULL);
    assert(rt_result_status(result) == RT_WRITE_STATUS_OK);
    assert(rt_result_value(result) == NULL);

    /* Verify by reading back */
    FILE *f = fopen(filename, "r");
    assert(f != NULL);
    char buffer[256];
    fgets(buffer, sizeof(buffer), f);
    fclose(f);
    assert(strcmp(buffer, content) == 0);

    /* Clean up */
    unlink(filename);

    printf("test_write_file_success: PASS\n");
}

/* Test writing an empty file */
static void
test_write_file_empty(void)
{
    const char *filename = "test_temp_write_empty.txt";
    const char *content = "";

    rt_result_t *result = rt_write_file(filename, content);
    assert(result != NULL);
    assert(rt_result_status(result) == RT_WRITE_STATUS_OK);

    /* Verify file exists and is empty */
    FILE *f = fopen(filename, "r");
    assert(f != NULL);
    fseek(f, 0, SEEK_END);
    long size = ftell(f);
    fclose(f);
    assert(size == 0);

    /* Clean up */
    unlink(filename);

    printf("test_write_file_empty: PASS\n");
}

/* Test writing a large file */
static void
test_write_file_large(void)
{
    const char *filename = "test_temp_write_large.txt";

    /* Build large content */
    char content[50000];
    content[0] = '\0';
    for (int i = 0; i < 500; i++) {
        strcat(content, "The quick brown fox jumps over the lazy dog. ");
    }

    rt_result_t *result = rt_write_file(filename, content);
    assert(result != NULL);
    assert(rt_result_status(result) == RT_WRITE_STATUS_OK);

    /* Verify file size */
    FILE *f = fopen(filename, "r");
    assert(f != NULL);
    fseek(f, 0, SEEK_END);
    long size = ftell(f);
    fclose(f);
    assert(size == (long) strlen(content));

    /* Clean up */
    unlink(filename);

    printf("test_write_file_large: PASS\n");
}

/* Test writing with NULL filename */
static void
test_write_file_null_filename(void)
{
    const char *content = "Some content";

    rt_result_t *result = rt_write_file(NULL, content);
    assert(result != NULL);
    assert(rt_result_status(result) == RT_WRITE_STATUS_INVALID_INPUT);
    assert(rt_result_value(result) == NULL);

    printf("test_write_file_null_filename: PASS\n");
}

/* Test writing with NULL data */
static void
test_write_file_null_data(void)
{
    const char *filename = "test_temp.txt";

    rt_result_t *result = rt_write_file(filename, NULL);
    assert(result != NULL);
    assert(rt_result_status(result) == RT_WRITE_STATUS_INVALID_INPUT);
    assert(rt_result_value(result) == NULL);

    printf("test_write_file_null_data: PASS\n");
}

/* Test round-trip: write then read */
static void
test_write_read_roundtrip(void)
{
    const char *filename = "test_temp_roundtrip.txt";
    const char *original = "Round-trip test!\nLine 2\nLine 3 with émojis 😀\n";

    /* Write */
    rt_result_t *write_result = rt_write_file(filename, original);
    assert(write_result != NULL);
    assert(rt_result_status(write_result) == RT_WRITE_STATUS_OK);

    /* Read */
    rt_result_t *read_result = rt_read_file(filename);
    assert(read_result != NULL);
    assert(rt_result_status(read_result) == RT_READ_STATUS_OK);

    char *data = rt_result_value(read_result);
    assert(data != NULL);
    assert(strcmp(data, original) == 0);

    /* Clean up */
    unlink(filename);

    printf("test_write_read_roundtrip: PASS\n");
}

/* Test result accessors with wrapper functions */
static void
test_result_wrappers(void)
{
    const char *filename = "test_temp_wrappers.txt";
    const char *content = "Testing wrappers!";

    /* Write using wrapper */
    rt_value_t filename_v = rt_string_box(rt_string_new(filename));
    rt_value_t content_v = rt_string_box(rt_string_new(content));
    rt_value_t write_result = coal_write_file(filename_v, content_v);

    rt_value_t write_status = coal_result_status(write_result);
    assert(rt_int32_unbox(write_status) == RT_WRITE_STATUS_OK);

    /* Read using wrapper */
    rt_value_t read_result = coal_read_file(filename_v);
    rt_value_t read_status = coal_result_status(read_result);
    assert(rt_int32_unbox(read_status) == RT_READ_STATUS_OK);

    rt_value_t data_v = coal_result_value(read_result);
    rt_string_t *data_str = rt_string_unbox(data_v);
    assert(strcmp(rt_string_data(data_str), content) == 0);

    /* Clean up */
    unlink(filename);

    printf("test_result_wrappers: PASS\n");
}

/* Test wrapper with non-existent file */
static void
test_wrapper_file_not_found(void)
{
    rt_value_t filename = rt_string_box(rt_string_new("nonexistent_xyz.txt"));
    rt_value_t result = coal_read_file(filename);
    rt_value_t status = coal_result_status(result);

    assert(rt_int32_unbox(status) == RT_READ_STATUS_FILE_NOT_FOUND);

    rt_value_t value = coal_result_value(result);
    assert(rt_ptr_unbox(value) == NULL);

    printf("test_wrapper_file_not_found: PASS\n");
}

/* Test overwriting an existing file */
static void
test_write_file_overwrite(void)
{
    const char *filename = "test_temp_overwrite.txt";
    const char *content1 = "First content";
    const char *content2 = "Second content that is longer";

    /* Write first time */
    rt_result_t *result1 = rt_write_file(filename, content1);
    assert(result1 != NULL);
    assert(rt_result_status(result1) == RT_WRITE_STATUS_OK);

    /* Write second time (overwrite) */
    rt_result_t *result2 = rt_write_file(filename, content2);
    assert(result2 != NULL);
    assert(rt_result_status(result2) == RT_WRITE_STATUS_OK);

    /* Verify only second content exists */
    rt_result_t *read_result = rt_read_file(filename);
    assert(read_result != NULL);
    assert(rt_result_status(read_result) == RT_READ_STATUS_OK);

    char *data = rt_result_value(read_result);
    assert(strcmp(data, content2) == 0);

    /* Clean up */
    unlink(filename);

    printf("test_write_file_overwrite: PASS\n");
}

int
main(void)
{
    rt_runtime_init();

    printf("Running file I/O tests...\n");

    /* Read file tests */
    test_read_file_success();
    test_read_file_not_found();
    test_read_file_empty();
    test_read_file_large();
    test_read_file_utf8();

    /* Write file tests */
    test_write_file_success();
    test_write_file_empty();
    test_write_file_large();
    test_write_file_null_filename();
    test_write_file_null_data();
    test_write_file_overwrite();

    /* Integration tests */
    test_write_read_roundtrip();

    /* Wrapper tests */
    test_result_wrappers();
    test_wrapper_file_not_found();

    printf("All file I/O tests passed!\n");

    return 0;
}
