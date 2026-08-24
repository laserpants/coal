#!/bin/bash
# Regression check for compiler issue #2 (let-bound values re-evaluated per use).
#
# Verifies strict-once (`let`) and strict (function parameter) evaluation
# semantics using an FFI counter fixture. A `let`-bound value must be
# evaluated EXACTLY ONCE at binding time; re-evaluating it at every use would
# multiply FFI side effects (e.g. registering duplicate resource handles).
#
# Not part of `stack test`: run manually with
#
#     COAL=/path/to/coal bash run.sh
#
# Expected output is in `.expected`:
#
#   * `before` -> 1     : the `let src = take(2)` RHS ran its FFI effect once.
#   * `after` -> 1      : the three polls of `src` did NOT re-run the RHS.
#   * `src.guard` -> 1  : the recorded guard id reflects the single construction.
#   * `poly_calls` -> 1 : a `let` whose RHS has an unresolved trait constraint
#                         (`poly_mk`, no return annotation) is ALSO evaluated
#                         exactly once. Without the value restriction in
#                         Coal.TypeSystem.Constraint.Generation this binding is
#                         generalized, InsertDictionaries rewrites it into a
#                         dictionary lambda, and each use of `x` re-applies it
#                         -> `poly_calls=2`. The annotated cases above cannot
#                         detect that, so this line is the one that guards the
#                         polymorphic path.
set -e
cd "$(dirname "$0")"

COAL="${COAL:-coal}"
"$COAL" build >/dev/null 2>&1

BIN=$(find . -maxdepth 1 -name 'let-evaluation-*' -type f | head -1)
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
    echo "PASS: let is strict-once on $COAL"
else
    echo "FAIL:" 1>&2
    echo "--- expected ---" 1>&2
    echo "$EXPECTED" 1>&2
    echo "--- actual ---" 1>&2
    echo "$ACTUAL" 1>&2
    exit 1
fi
