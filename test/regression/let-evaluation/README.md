# let-evaluation — strict-`let` regression fixture

Guards strict-once evaluation of `let`: a binding's right-hand side must be
evaluated **exactly once**, at the binding site — never once per use. A
re-evaluated binding would multiply side effects (e.g. duplicate resource
registration), which pure-Coal E2E examples cannot observe. Hence the C
counter fixture (`counter.c`) and the expected counts in `.expected`.

> **Not part of `stack test`.** Run it manually:

    COAL=/path/to/coal bash run.sh

`PASS: let is strict-once on …` means every binding's RHS ran exactly once.
Growth of any count with the number of uses fails with an expected/actual
diff.

## The case that matters

`before` / `after` / `src.guard` / `polls` / `param_driven` cover plain
strict-once behaviour with fully annotated types; they pass even on a broken
compiler.

`poly_calls` is the line that catches the regression this fixture exists for:
`poly_mk` has *no* return annotation, so its `5` infers as `a with
Numeric<a>`. If the `let` in `poly_driven` generalizes over that trait
constraint, dictionary insertion rewrites the binding into a dictionary
lambda and re-applies it at every use of `x`, so `mk()` runs twice
(`poly_calls=2`). The fix is the value restriction in
`Coal.TypeSystem.Constraint.Generation` (`isExpansive` / `letAssertion`),
which keeps an expansive right-hand side monomorphic so it evaluates exactly
once. Removing the annotation from `poly_mk` is essential: with a concrete
return type no trait constraint survives and the dictionary-lambda path never
fires.

## Artifacts

Running leaves `.build/`, `coal.lock.json`, and the produced
`let-evaluation-*` binary here; all three are covered by the repository
`.gitignore`. `run.sh` deletes the binary again on success and keeps
everything for inspection on failure.
