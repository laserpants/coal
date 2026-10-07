# ffi-in-return — foreign-call-in-`return` regression fixture

Guards that wrapping a foreign (FFI) call **directly** in `return(...)` emits
the foreign call exactly once.

`return` is the imported `IO.return`, an identity function at runtime. Inside a
lambda, lambda lifting captures that imported name as a free variable, so at
codegen time the callee of `return(#{ffi}(...))` is a **local closure** rather
than a saturated global call. `Coal.Kernel.LLVM.Codegen.irValue` used to
evaluate the arguments of such a non-saturated call twice — once eagerly (the
result discarded) and once via `irPackArgs` (the result used) — so the foreign
call ran twice. The duplicated call's side effect is not observable from pure
Coal, hence the C counter fixture (`counter.c`).

> **Not part of `stack test`.** Run it manually:

    COAL=/path/to/coal bash run.sh

`PASS: ffi-in-return emits the foreign call once on …` means the poll ran its
FFI effect exactly once (`calls=1`). A doubled call yields `calls=2`.

## The case that matters

The lambda must be passed as an **argument** (`apply_poll(fn(_) => ...)`) so it
is lambda-lifted. A top-level `fun buggy() = return(#{ffi}(...))` whose body is
the expression is *not* affected: `return` stays a global there, the call is
saturated, and it is already emitted once.

## Artifacts

Running leaves `.build/`, `coal.lock.json`, and the produced
`ffi-in-return-*` binary here; all three are covered by the repository
`.gitignore`. `run.sh` deletes the binary again on success and keeps everything
for inspection on failure.
