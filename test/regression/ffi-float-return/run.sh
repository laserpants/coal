#!/bin/bash
# Regression check: float/double foreign-call results must be returned
# unboxed, matching the code generator's foreign-call ABI (see README.md).
#
# Not part of `stack test`: run manually with
#
#     COAL=/path/to/coal bash run.sh
#
# Expected output is in `.expected`.
set -e
cd "$(dirname "$0")"

COAL="${COAL:-coal}"
"$COAL" build >/dev/null 2>&1

BIN=$(find . -maxdepth 1 -name 'ffi-float-return-*' -type f | head -1)
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
    echo "PASS: ffi-float-return returns float/double values correctly on $COAL"
else
    echo "FAIL:" 1>&2
    echo "--- expected ---" 1>&2
    echo "$EXPECTED" 1>&2
    echo "--- actual ---" 1>&2
    echo "$ACTUAL" 1>&2
    exit 1
fi
