# Debugging

## Mandatory workflow

When debugging compiler problems, follow this workflow in order. Do not skip
steps or propose code changes before completing the investigation.

1. **Gather evidence.** Reproduce the problem. Capture exact input, output,
   error messages, and exit codes. Identify which pipeline phase produces the
   first incorrect result.
2. **Explain observations.** Describe what happens vs. what should happen.
   Be precise: "the kernel IR after lambda lifting has a free variable `x.3`
   that should have been closed."
3. **Produce hypotheses.** List concrete possible causes. Rank them by
   plausibility. A hypothesis must predict a specific observable difference.
4. **Compare old and new implementations.** If the new pipeline diverges from
   legacy behaviour, compare the legacy pass (`src/Coal/LegacyKernel/Compiler.hs`)
   with the corresponding new pass (`src/Coal/Kernel/Pipeline/Pass/`). Isolate
   the specific transformation step that differs.
5. **Identify the smallest semantic difference.** Reduce the input program to
   a minimal example that still exhibits the problem. The minimal example
   should ideally be a single function or expression.
6. **Only then propose code changes.** The fix must explain which invariant
   was violated and why the change restores it.

## Avoid speculative edits

Do not make changes without understanding the root cause. If multiple fixes
are possible, test each hypothesis against the legacy compiler's output before
committing to one.

## Key invariants to check

- **ANF**: After `controlFlowNorm`, every non-atomic sub-expression must be a
  let-binding.
- **Constructor saturation**: All constructor applications are fully saturated.
- **Lambda flattening**: No nested `fn(a) => fn(b) => ...` remain.
- **Name uniqueness**: After `localNameCanonicalization`, every local name is
  globally unique within its module.

## Test infrastructure

- End-to-end tests: `test/E2E/Spec.hs` — runs example Coal programs and checks
  their output.
- Individual test programs: `test/Coal/examples/` — numbered directories
  containing `Main.coal` files with expected output in `.expected` files.
- Do **not** run `stack test` (takes too long). Run individual tests or hand
  control back to the user to evaluate.