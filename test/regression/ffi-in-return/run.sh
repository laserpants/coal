#!/bin/bash
# Regression check: a foreign (FFI) call wrapped directly in `return(...)`
# inside a lambda must be emitted exactly once (see README.md).
#
# Not part of `stack test`: run manually with
#
#     COAL=/path/to/coal bash run.sh
#
# Expected output is in `.expected`:
#
#   * `calls=1` : the single poll ran its FFI effect exactly once. A doubled
#                 call yields `calls=2`.
set -e
cd "$(dirname "$0")"

COAL="${COAL:-coal}"
"$COAL" build >/dev/null 2>&1

BIN=$(find . -maxdepth 1 -name 'ffi-in-return-*' -type f | head -1)
if [ -z "$BIN" ]; then
    echo "FAIL: no executable produced by 'coal build'" 1>&2
    exit 1
fi

ACTUAL=$(timeout 10 "./$BIN")
EXPECTED=$(cat .expected)

if [ "$ACTUAL" == "$EXPECTED" ]; then
    # Keep the tree tidy: drop the produced binary on success. `.build/` and
    # `coal.lock.json` are kept so subsequent runs stay incremental.
    rm -f "$BIN"
    echo "PASS: ffi-in-return emits the foreign call once on $COAL"
else
    echo "FAIL:" 1>&2
    echo "--- expected ---" 1>&2
    echo "$EXPECTED" 1>&2
    echo "--- actual ---" 1>&2
    echo "$ACTUAL" 1>&2
    exit 1
fi
