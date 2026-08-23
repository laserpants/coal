# let-evaluation — strict-`let` regression fixture

Regression guard for compiler issue #2
(`compiler-issues/02-let-binding-re-evaluated-per-use.md`): a `let`-bound
value whose right-hand side performs an FFI side effect must be evaluated
**exactly once** (strict-once), not once per use. Pure-Coal E2E examples
cannot observe evaluation counts, hence the C counter fixture
(`counter.c`) and the expected counts in `.expected`.

> **Not part of `stack test`.** Run it manually:

    COAL=/path/to/coal bash run.sh

`PASS: let is strict-once on …` means the binding's RHS ran exactly once and
the three subsequent uses did not re-run it. Any growth of the counts with
the number of uses (call-by-name without memoization) fails with an
expected/actual diff.

## Artifacts

Running leaves `.build/`, `coal.lock.json`, and the produced
`let-evaluation-*` binary here; all three are covered by the repository
`.gitignore`. `run.sh` deletes the binary again on success and keeps
everything for inspection on failure.
