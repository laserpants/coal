# blocking-poll-loop — constant-stack `Runtime.blocking_poll` regression fixture

Guards that `Runtime.blocking_poll` (builtin `event$_blocking_poll`) runs its
try/block/retry loop in **constant C stack**, even when `block_fn` returns
without blocking.

`blocking_poll` is represented as `io_defer(fn(_) => ...)` whose loop retries by
evaluating `io_eval(blocking_poll(state, try_fn, block_fn))`. Because the
recursive call is an *argument* to `io_eval`, it is not in tail position: the
generated code emits a plain `call` (not a `tail call`), so every iteration
pushes frames until the 8 MiB stack overflows. The fix splits each IO loop into
a raw-result, self-tail-recursive helper (`event$_blocking_poll_loop`,
`event$_loop_io_loop`) plus a thin `io_defer` wrapper, so the recursion becomes a
true tail call that LLVM's `TailCallElim` folds into a loop.

> **Not part of `stack test`.** Run it manually:

    COAL=/path/to/coal bash run.sh

`PASS: blocking_poll is constant-stack on …` means the loop ran to completion.

## The case that matters

`try_fn` returns `None` until the C counter (`counter.c`) reaches `LIMIT`, then
`Some(n)`. `block_fn` returns immediately (never blocks) — the adversarial
situation that previously turned the retry into an unbounded recursion. On a
recursive implementation the process dies with SIGSEGV long before `LIMIT`; the
fixture's `run.sh` therefore treats a non-zero exit status as a failure, not
just a mismatched value.

## Artifacts

Running leaves `.build/`, `coal.lock.json`, and the produced
`blocking-poll-loop-*` binary here; all three are covered by the repository
`.gitignore`. `run.sh` deletes the binary again on success and keeps everything
for inspection on failure.
