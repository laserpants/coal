#!/bin/bash
# Combine all runtime modules into a single C file

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
SRC_DIR="$ROOT_DIR/src"
INCLUDE_DIR="$ROOT_DIR/include/coal"
OUTPUT_FILE="$ROOT_DIR/dist/runtime-combined.c"

echo "Combining runtime modules into $OUTPUT_FILE"

# Create output directory if needed
mkdir -p "$(dirname "$OUTPUT_FILE")"

# Start output file
cat > "$OUTPUT_FILE" << 'EOF'
/*
 * Combined Coal Runtime Library
 * Auto-generated - DO NOT EDIT
 *
 * This file is generated from runtime/ source modules.
 * To regenerate: cd runtime && ./scripts/combine.sh
 */

/* ============================================================================
 * External library includes
 * ============================================================================
 */
#include <gc.h>
#include <gmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <stddef.h>
#include <wctype.h>
#include <time.h>
#include <errno.h>
#include <inttypes.h>

/* ============================================================================
 * Coal runtime headers (inlined)
 * ============================================================================
 */

EOF

# Array of headers in dependency order
HEADERS=(
    "panic.h"
    "gc.h"
    "math.h"
    "char.h"
    "value.h"
    "bignum.h"
    "closure.h"
    "record.h"
    "string.h"
    "apply.h"
    "io.h"
    "runtime.h"
    "value_api.h"
)

# Inline headers, removing include guards and coal/ includes
for header in "${HEADERS[@]}"; do
    echo "/* --- coal/$header --- */" >> "$OUTPUT_FILE"

    # Process header: remove include guards completely
    # 1. Remove #ifndef COAL_*_H line
    # 2. Remove #define COAL_*_H line
    # 3. Remove coal/ includes (already inlined) - both "coal/foo.h" and "foo.h" patterns
    # 4. Remove the closing #endif (last line of header guard)
    sed -e '/^#ifndef COAL_.*_H$/d' \
        -e '/^#define COAL_.*_H$/d' \
        -e '/^#include "coal\//d' \
        -e '/^#include "[a-z_]*\.h"$/d' \
        -e '/^#endif$/d' \
        "$INCLUDE_DIR/$header" >> "$OUTPUT_FILE"

    echo "" >> "$OUTPUT_FILE"
done

# Add source files section
cat >> "$OUTPUT_FILE" << 'EOF'

/* ============================================================================
 * Coal runtime implementation
 * ============================================================================
 */

EOF

# Array of source files in dependency order
SOURCES=(
    "panic.c"
    "gc.c"
    "math.c"
    "char.c"
    "value.c"
    "bignum.c"
    "closure.c"
    "record.c"
    "string.c"
    "apply.c"
    "io.c"
    "runtime.c"
    "value_api.c"
)

# Concatenate source files, removing coal/ includes
for source in "${SOURCES[@]}"; do
    echo "/* --- $source --- */" >> "$OUTPUT_FILE"

    # Remove #include "coal/..." lines since headers are already inlined
    sed -e '/^#include "coal\//d' \
        -e '/^#include <gc\.h>/d' \
        -e '/^#include <gmp\.h>/d' \
        -e '/^#include <stdio\.h>/d' \
        -e '/^#include <stdlib\.h>/d' \
        -e '/^#include <stdint\.h>/d' \
        -e '/^#include <stdbool\.h>/d' \
        -e '/^#include <string\.h>/d' \
        -e '/^#include <stddef\.h>/d' \
        -e '/^#include <wctype\.h>/d' \
        -e '/^#include <time\.h>/d' \
        -e '/^#include <errno\.h>/d' \
        -e '/^#include <inttypes\.h>/d' \
        "$SRC_DIR/$source" >> "$OUTPUT_FILE"

    echo "" >> "$OUTPUT_FILE"
done

echo "Successfully combined runtime into $OUTPUT_FILE"
echo "Lines: $(wc -l < "$OUTPUT_FILE")"

# Test compilation
echo "Testing compilation..."
if clang -std=c11 -Wall -Wextra -Wpedantic -c "$OUTPUT_FILE" \
    -o "$ROOT_DIR/dist/runtime-combined.o" \
    $(pkg-config --cflags bdw-gc gmp 2>/dev/null || echo "-I/opt/homebrew/include") 2>&1 | head -20; then
    echo "✓ Combined runtime compiles successfully"
    rm -f "$ROOT_DIR/dist/runtime-combined.o"
else
    echo "✗ Combined runtime has compilation errors"
    exit 1
fi