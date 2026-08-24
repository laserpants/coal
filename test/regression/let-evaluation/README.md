# let-evaluation — strict-`let` regression fixture

Regression guard for compiler issue #2: a `let`-bound value whose right-hand
side performs an FFI side effect must be evaluated **exactly once**
(strict-once), not once per use. Pure-Coal E2E examples cannot observe
evaluation counts, hence the C counter fixture (`counter.c`) and the expected
counts in `.expected`.

> **Not part of `stack test`.** Run it manually:

    COAL=/path/to/coal bash run.sh

`PASS: let is strict-once on …` means each binding's RHS ran exactly once and
the subsequent uses did not re-run it. Any growth of the counts with the
number of uses (call-by-name without memoization) fails with an
expected/actual diff.

## The two cases

`before` / `after` / `src.guard` cover a **monomorphic** `let` (`take` has a
concrete `Source<int32>` return annotation).

`poly_calls` covers the **polymorphic** case, and is the line that actually
catches issue #2. `poly_mk` has *no* return annotation, so its `5` is inferred
as `a with Numeric<a>`. If the `let` in `poly_driven` generalizes over that
constraint, `InsertDictionaries` rewrites the binding into a dictionary lambda
and re-applies it at every use of `x`, giving `poly_calls=2`. The fix is the
value restriction in `Coal.TypeSystem.Constraint.Generation` (`isExpansive` /
`letAssertion`), which keeps an expansive right-hand side monomorphic.

Removing the annotation is essential: with a concrete return type no trait
constraint survives and the dictionary-lambda path never fires, so the
monomorphic cases alone pass even on a broken compiler.

## Artifacts

Running leaves `.build/`, `coal.lock.json`, and the produced
`let-evaluation-*` binary here; all three are covered by the repository
`.gitignore`. `run.sh` deletes the binary again on success and keeps
everything for inspection on failure.
