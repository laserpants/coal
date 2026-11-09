#!/bin/bash
# Regression check: Runtime.blocking_poll must run its retry loop in constant
# stack, even when `block_fn` never blocks (see README.md).
#
# The fixture's `try_fn` returns None until a C counter reaches LIMIT, then
# Some(n); `block_fn` returns immediately. A recursive blocking_poll overflows
# the C stack (SIGSEGV) long before LIMIT; a constant-stack loop prints the
# result.
#
# Not part of `stack test`: run manually with
#
#     COAL=/path/to/coal bash run.sh
set -e
cd "$(dirname "$0")"

COAL="${COAL:-coal}"
"$COAL" build >/dev/null 2>&1

BIN=$(find . -maxdepth 1 -name 'blocking-poll-loop-*' -type f | head -1)
if [ -z "$BIN" ]; then
    echo "FAIL: no executable produced by 'coal build'" 1>&2
    exit 1
fi

set +e
ACTUAL=$(timeout 60 "./$BIN")
STATUS=$?
set -e

if [ "$STATUS" -ne 0 ]; then
    echo "FAIL: executable exited with status $STATUS (stack overflow/SIGSEGV?)" 1>&2
    exit 1
fi

EXPECTED=$(cat .expected)
if [ "$ACTUAL" == "$EXPECTED" ]; then
    # Keep the tree tidy: drop the produced binary on success. `.build/` and
    # `coal.lock.json` are kept so subsequent runs stay incremental.
    rm -f "$BIN"
    echo "PASS: blocking_poll is constant-stack on $COAL"
else
    echo "FAIL:" 1>&2
    echo "--- expected ---" 1>&2
    echo "$EXPECTED" 1>&2
    echo "--- actual ---" 1>&2
    echo "$ACTUAL" 1>&2
    exit 1
fi
