# Coding style guide

This document defines the C11 coding standards and style conventions for this project. Following these guidelines ensures consistency and maintainability across the codebase.

---

## Table of contents

1. [General principles](#general-principles)
2. [Language standard](#language-standard)
3. [File organization](#file-organization)
4. [Naming conventions](#naming-conventions)
5. [Formatting](#formatting)
6. [Types and declarations](#types-and-declarations)
7. [Functions](#functions)
8. [Memory management](#memory-management)
9. [Error handling](#error-handling)
10. [Comments and documentation](#comments-and-documentation)
11. [Best practices](#best-practices)

---

## General principles

- **Clarity over cleverness**: Write code that is easy to understand
- **Consistency**: Follow existing patterns in the codebase
- **Safety**: Prefer defensive programming and explicit error handling
- **Portability**: Write standard C11 code; avoid platform-specific extensions
- **Simplicity**: Keep functions focused and modules cohesive

---

## Language standard

### C11 compliance

- Use **C11** as the baseline standard (`-std=c11`)
- Compile with strict warnings: `-Wall -Wextra -Wpedantic -Werror`
- Use standard headers from C11:
  - `<stdint.h>` for fixed-width integers
  - `<stdbool.h>` for boolean types
  - `<stddef.h>` for `size_t`, `NULL`, etc.
  - `<inttypes.h>` for format macros (`PRId32`, `PRId64`)

### Allowed C11 features

✅ **Use these C11 features:**
- Fixed-width integer types (`int32_t`, `uint32_t`, `int64_t`)
- Boolean type (`bool`, `true`, `false`)
- `_Noreturn` function specifier
- Flexible array members (`type field[]`)
- `_Static_assert` for compile-time checks
- Anonymous unions/structs (if needed)
- Designated initializers

❌ **Avoid these:**
- VLAs (Variable Length Arrays) - use dynamic allocation instead
- `alloca()` - not standard C11, use heap allocation
- Compiler-specific extensions (unless properly guarded)

---

## File organization

### Header files (`include/coal/*.h`)

**Structure:**
```c
#ifndef COAL_MODULE_H
#define COAL_MODULE_H

/* Minimal includes - prefer forward declarations */
#include <stdint.h>
#include <stdbool.h>

/* Forward declarations for opaque types */
typedef struct rt_type rt_type_t;

/* Function declarations */
rt_type_t *rt_type_create(int32_t value);
void rt_type_destroy(rt_type_t *obj);

#endif
```

**Rules:**
- Always use include guards (`#ifndef COAL_MODULE_H`)
- Include only what's necessary in headers
- Use forward declarations for opaque types
- No function implementations except `static inline`
- Self-contained: header can be included independently

### Source files (`src/*.c`)

**Structure:**
```c
#include "coal/module.h"
#include "coal/gc.h"
#include <stdlib.h>
#include <string.h>

/* Struct definitions (if not in header) */
typedef struct rt_type {
    int32_t value;
    char data[];
} rt_type_t;

/* Static helper functions */
static int
helper_function(int x)
{
    return x * 2;
}

/* Public API implementations */
rt_type_t *
rt_type_create(int32_t value)
{
    /* Implementation */
}
```

**Rules:**
- Include corresponding header first
- Group includes: coal headers, system headers
- Define opaque structs at file scope
- Mark internal helpers as `static`
- Order: struct definitions → static helpers → public functions

---

## Naming conventions

### Prefixes

All public API uses the `rt_` (runtime) prefix:

| Prefix | Usage | Example |
|--------|-------|---------|
| `rt_` | Public runtime functions | `rt_string_new()` |
| `rt_` | Public runtime types | `rt_string_t`, `rt_value_t` |
| `coal_` | Wrapper functions (boxing API) | `coal_string_concat()` |
| `COAL_` | Header guards | `#ifndef COAL_STRING_H` |
| `RT_` | Public constants | `RT_READ_STATUS_SUCCESS` |

### Function names

**Format**: `rt_module_action()` or `rt_type_action()`

```c
// Module-based
rt_gc_init()          // Initialize GC module
rt_panic()            // Panic from panic module

// Type-based
rt_string_new()       // Create new string
rt_bignum_add()       // Add two bignums
rt_record_lookup()    // Lookup in record
```

**Rules:**
- Lowercase with underscores (`snake_case`)
- Start with `rt_` prefix
- Followed by module or type name
- Ended with action verb
- Be descriptive: `rt_string_concat()` not `rt_str_cat()`

### Type names

**Format**: `rt_name_t` for types

```c
typedef struct rt_string rt_string_t;
typedef struct rt_closure rt_closure_t;
typedef void *rt_value_t;
```

**Rules:**
- Always end with `_t` suffix
- Use descriptive names
- Struct tags match typedef name (without `_t`)

### Variable names

```c
// Good
rt_string_t *result;
int32_t total_args;
size_t buffer_size;
const char *field_name;

// Avoid
rt_string_t *r;        // Too short
int32_t tArgs;         // camelCase
size_t BUFFER_SIZE;    // SCREAMING_CASE for non-constants
```

**Rules:**
- Lowercase with underscores
- Descriptive names (avoid single letters except loop counters)
- No Hungarian notation
- Use `i`, `j`, `k` only for simple loop counters

### Constants and macros

```c
// Constants
#define MAX_BUFFER_SIZE 1024
#define UTF8_1BYTE_MAX 0x7F

// Status codes
#define RT_READ_STATUS_SUCCESS 0
#define RT_READ_STATUS_FILE_NOT_FOUND 1
```

**Rules:**
- `SCREAMING_SNAKE_CASE` for preprocessor constants
- `RT_` prefix for public constants
- No lowercase macros (reserved for functions)

---

## Formatting

### Automated formatting

The project uses `clang-format` with the LLVM style as a base. Run:

```bash
clang-format -i src/*.c include/coal/*.h
```

### Indentation

- **4 spaces** (no tabs) for C source files
- **Tabs** for Makefiles
- Continuation lines: 4 spaces

```c
rt_string_t *
rt_string_concat(rt_string_t *a, rt_string_t *b)
{
    if (condition) {
        do_something();
    }
}
```

### Line length

- **Maximum 80 characters** per line
- Break long lines logically

```c
// Good
rt_value_t result =
    rt_apply(closure, remaining_argc, remaining_args);

// Also acceptable
rt_value_t result = rt_apply(
    closure,
    remaining_argc,
    remaining_args
);
```

### Braces

**Linux/K&R style** - opening brace on same line, except functions:

```c
// Functions: brace on new line
void
rt_function(void)
{
    /* body */
}

// Control structures: brace on same line
if (condition) {
    statement;
} else {
    statement;
}

while (condition) {
    statement;
}

for (int i = 0; i < n; i++) {
    statement;
}
```

**Rules:**
- Always use braces, even for single statements
- No single-line functions or control structures

### Spacing

```c
// After casts
value = (int32_t) x;
ptr = (char *) buffer;

// Around operators
int result = a + b * c;
if (x == 5 && y > 10) { }

// In function calls
func(arg1, arg2, arg3);

// No spaces inside parentheses
if (condition) { }        // Good
if ( condition ) { }      // Bad
```

### Pointer declaration

**Pointer asterisk aligned right** (with type):

```c
// Good
char *string;
rt_value_t *array;
const rt_string_t *str;

// Avoid
char* string;
char * string;
```

---

## Types and declarations

### Integer types

**Always use fixed-width types** from `<stdint.h>`:

```c
// Preferred
int32_t count;
int64_t offset;
uint32_t codepoint;
size_t length;

// Avoid
int count;             // Ambiguous size
long offset;           // Platform-dependent
unsigned codepoint;    // Not explicit about width
```

### Boolean type

Use `bool` from `<stdbool.h>`:

```c
#include <stdbool.h>

bool rt_string_compare(rt_string_t *a, rt_string_t *b);

bool is_valid = true;
if (is_valid) { }
```

### Const correctness

Mark read-only parameters and variables as `const`:

```c
// Good - parameters not modified
rt_string_t *
rt_string_concat(const rt_string_t *a, const rt_string_t *b);

void
rt_print_string(const char *s);

// Good - pointer to const data
const char *
rt_string_data(const rt_string_t *s);
```

### Forward declarations

Use opaque pointers for encapsulation:

```c
// In header: forward declaration
typedef struct rt_string rt_string_t;

// In source: full definition
typedef struct rt_string {
    int64_t length;
    char data[];
} rt_string_t;
```

### Flexible array members

Use C11 flexible array members for variable-sized structures:

```c
typedef struct rt_string {
    int64_t length;
    char data[];        // Flexible array member
} rt_string_t;

// Allocate with extra space
size_t size = sizeof(rt_string_t) + len + 1;
rt_string_t *str = rt_alloc_atomic(size);
```

---

## Functions

### Function signatures

**Format:** Return type on separate line for definitions:

```c
// Header declaration
rt_string_t *rt_string_new(const char *s);

// Source definition
rt_string_t *
rt_string_new(const char *s)
{
    /* implementation */
}
```

### Parameter order

**Convention:** Output before input, self before others:

```c
// Good
void rt_string_copy(char *dest, const char *src, size_t n);
rt_value_t rt_record_lookup(rt_record_t *record, const char *field);

// Consistent with standard library (memcpy, etc.)
```

### Static functions

Mark internal helper functions as `static`:

```c
// Only used within this file
static int
utf8_char_len(unsigned char first_byte)
{
    /* implementation */
}
```

### Inline functions

Use `static inline` for small performance-critical functions:

```c
// In header
static inline rt_value_t
rt_int32_box(int32_t n)
{
    return (void *) (uintptr_t) n;
}
```

**Guidelines:**
- Keep inline functions small (≤ 5 lines)
- Only for frequently called, simple operations
- Boxing/unboxing functions are good candidates

### Function length

- **Prefer functions under 50 lines**
- Extract complex logic into helper functions
- One function = one responsibility

---

## Memory management

### Allocation

**Use Boehm GC functions** via runtime wrappers:

```c
#include "coal/gc.h"

// For structures with pointers
rt_string_t *str = rt_alloc(sizeof(rt_string_t));

// For atomic data (no internal pointers)
char *buffer = rt_alloc_atomic(size);
```

**Rules:**
- Never use `malloc()`, `calloc()`, `realloc()`, or `free()`
- Use `rt_alloc()` for structures containing pointers
- Use `rt_alloc_atomic()` for data without pointers (strings, numbers)
- Always check allocation results for NULL

### NULL checks

**Always check allocation results:**

```c
rt_string_t *str = rt_alloc_atomic(size);
if (!str) {
    rt_panic("Out of memory");
}
```

### Integer overflow

**Check size calculations before allocation:**

```c
// Bad - potential overflow
size_t size = sizeof(rt_string_t) + len + 1;

// Good - check for overflow
if (len > SIZE_MAX - sizeof(rt_string_t) - 1) {
    rt_panic("String too large");
}
size_t size = sizeof(rt_string_t) + len + 1;
```

---

## Error handling

### Panic function

**Use `rt_panic()` for unrecoverable errors:**

```c
#include "coal/panic.h"

if (record == NULL) {
    rt_panic("Null record pointer");
}

if (field_not_found) {
    rt_panic("Record field not found (compiler bug)");
}
```

**When to panic:**
- NULL pointer violations
- Out of memory conditions
- Compiler bugs (invariant violations)
- Unreachable code paths

### Return values

**Use return values for expected errors:**

```c
// Return NULL for "not found" or similar
rt_value_t rt_record_lookup_safe(rt_record_t *record, const char *field)
{
    // Returns NULL if field doesn't exist
}

// Return status codes
typedef struct rt_result {
    int32_t status;
    char *value;
} rt_result_t;

rt_result_t *rt_read_file(const char *filename);
```

### Input validation

**Validate public API parameters:**

```c
rt_string_t *
rt_string_head(rt_string_t *s)
{
    if (!s) {
        rt_panic("NULL string pointer");
    }

    if (s->length == 0) {
        return 0;  // Empty string is valid
    }

    return utf8_decode(s->data);
}
```

---

## Comments and documentation

### Comment style

**Use C-style comments** (`/* */`) for consistency:

```c
/* Single-line comment */

/*
 * Multi-line comment with proper formatting.
 * Each line starts with asterisk for readability.
 */
```

**Note:** C++-style `//` comments are acceptable for inline notes:

```c
int total = a + b;  // Sum of inputs
```

### Function comments

**Document public API functions:**

```c
/**
 * Create a new string from a C string.
 *
 * Parameters:
 *   s - Null-terminated C string (must not be NULL)
 *
 * Returns:
 *   New rt_string_t allocated with GC
 *
 * Panics:
 *   If s is NULL or out of memory
 */
rt_string_t *
rt_string_new(const char *s)
{
    /* implementation */
}
```

### Header comments

**Document intent and invariants:**

```c
/**
 * Flexible array member pattern - actual size is
 * sizeof(rt_string_t) + length + 1
 */
typedef struct rt_string {
    int64_t length;
    char data[];
} rt_string_t;
```

### TODO comments

```c
/* TODO: Add bounds checking for large allocations */
/* FIXME: This assumes little-endian byte order */
/* NOTE: GC_malloc may not return NULL on OOM */
```

---

## Best practices

### UTF-8 handling

When working with Unicode text:

```c
// Good - UTF-8 aware
int32_t len = utf8_char_len(first_byte);
uint32_t cp = utf8_decode(str);

// Bad - assumes ASCII
int32_t len = 1;
uint32_t cp = str[0];
```

### String literals

**Use const for string literals:**

```c
const char *message = "Hello, World!";
rt_print_string("Error occurred");
```

### Magic numbers

**Avoid magic numbers - use named constants:**

```c
// Bad
if (cp <= 0x7F) { }
buffer = rt_alloc_atomic(128);

// Good
#define UTF8_1BYTE_MAX 0x7F
#define READLN_INITIAL_CAPACITY 128

if (cp <= UTF8_1BYTE_MAX) { }
buffer = rt_alloc_atomic(READLN_INITIAL_CAPACITY);
```

### Assertions

**Use static assertions for compile-time checks:**

```c
_Static_assert(sizeof(int32_t) == 4, "int32_t must be 4 bytes");
_Static_assert(sizeof(rt_string_t) == sizeof(int64_t),
               "Flexible array should not add to struct size");
```

### Avoid

❌ **Do not use:**
- Global mutable state
- `goto` (except for cleanup in error paths)
- Type punning (violates strict aliasing)
- Unnecessary type casts from `void*` in C
- Platform-specific code without guards
- `alloca()` - use heap allocation

### Portability

**Write portable code:**

```c
// Good - portable
#include <inttypes.h>
printf("%" PRId64 "\n", value);

// Bad - assumes long is 64-bit
printf("%ld\n", value);
```

---

## Code review checklist

Before submitting code, verify:

- [ ] Compiles with `-Wall -Wextra -Wpedantic -Werror`
- [ ] Follows naming conventions (`rt_*` prefix)
- [ ] NULL checks after all allocations
- [ ] Const correctness for read-only parameters
- [ ] Overflow checks before size calculations
- [ ] Functions are < 50 lines where reasonable
- [ ] Static functions for internal helpers
- [ ] Comments explain "why", not "what"
- [ ] No magic numbers
- [ ] UTF-8 aware for string operations
- [ ] Error handling is consistent (`rt_panic()` or return codes)
- [ ] Formatted with `clang-format`

---

## Tools and configuration

### Clang-format

Format code automatically:

```bash
# Format all files
clang-format -i src/*.c include/coal/*.h

# Check formatting (CI/CD)
clang-format --dry-run --Werror src/*.c include/coal/*.h
```

### Editorconfig

Ensure your editor respects `.editorconfig` for:
- LF line endings
- UTF-8 encoding
- 4-space indentation
- Trailing whitespace removal

### Compilation

```bash
# Development
make clean && make

# With sanitizers
clang -fsanitize=address -fsanitize=undefined -g -std=c11 ...

# Static analysis
clang --analyze -Xanalyzer -analyzer-output=text src/*.c
```

---

## Examples

### Good example

```c
/* string.c - Good style example */

#include "coal/string.h"
#include "coal/gc.h"
#include <string.h>

typedef struct rt_string {
    int64_t length;
    char data[];
} rt_string_t;

static const size_t STRING_MAX_LENGTH = SIZE_MAX - sizeof(rt_string_t) - 1;

/*
 * Create a new string from a C string.
 * Allocates memory using GC.
 */
rt_string_t *
rt_string_new(const char *s)
{
    if (!s) {
        rt_panic("NULL string pointer");
    }

    size_t len = strlen(s);
    if (len > STRING_MAX_LENGTH) {
        rt_panic("String too large");
    }

    size_t size = sizeof(rt_string_t) + len + 1;
    rt_string_t *str = rt_alloc_atomic(size);
    if (!str) {
        rt_panic("Out of memory");
    }

    str->length = (int64_t) len;
    memcpy(str->data, s, len + 1);

    return str;
}
```

### Bad example

```c
/* BAD EXAMPLE - DO NOT FOLLOW */

#include "string.h"  // Wrong path

// Missing typedef struct
struct String {
    long len;        // Use int64_t
    char* d;         // Use flexible array
};

// No documentation
// Wrong indentation (2 spaces)
// No NULL check
// No overflow check
struct String*
  string_new(char* s) {  // Missing rt_ prefix
  long l=strlen(s);      // No spaces around =
  struct String* str=(struct String*)malloc(sizeof(struct String));
  str->len=l;
  str->d=strdup(s);      // Memory leak - not GC tracked
  return str;
}
```

---

## References

- [C11 Standard (ISO/IEC 9899:2011)](https://www.iso.org/standard/57853.html)
- [LLVM Coding Standards](https://llvm.org/docs/CodingStandards.html)
- [Linux Kernel Coding Style](https://www.kernel.org/doc/html/latest/process/coding-style.html)
- [EditorConfig](https://editorconfig.org/)
- [Clang-Format](https://clang.llvm.org/docs/ClangFormat.html)
