# Compiler design principles and invariants

This document captures the design principles and invariants that guide ongoing
development.

## Design principles

- **Correctness before optimization.** Do not sacrifice behavioural fidelity for
  performance or code cleanliness. Get it right first.
- **Pipeline isolation.** Each compiler pass should have a single, well-defined
  responsibility. Passes are composed sequentially; each pass depends on the
  invariants established by prior passes.
- **Pure transformations where possible.** Normalization passes operate purely
  within the `PipelineT` monad (`StateT` over `ExceptT`). IO is only used at
  the outermost level for file I/O.
- **Immutable intermediate representations.** Each phase produces a new
  representation; phases do not mutate shared state (beyond the metadata
  environment).

## Key invariants

- **ANF**: After `controlFlowNorm`, every non-atomic sub-expression must be a
  let-binding.
- **Constructor saturation**: All constructor applications are fully saturated.
- **Lambda flattening**: No nested `fn(a) => fn(b) => ...` remain.
- **Name uniqueness**: After `localNameCanonicalization`, every local name is
  globally unique within its module.

## Pipeline contract

Each normalization pass must preserve the semantics of the input program. If a
pass transforms a module from `M` to `M'`, then `M` and `M'` must evaluate to
the same result. Passes may only strengthen invariants (e.g., by extracting
sub-expressions into let-bindings), never weaken them.

## Test infrastructure

- End-to-end tests: `test/E2E/Spec.hs` — runs example Coal programs and checks
  their output.
- Individual test programs: `test/examples/` — numbered directories containing
  `Main.coal` files with expected output in `.expected` files.
- Do **not** run `stack test` (takes too long). Run individual tests or hand
  control back to the user to evaluate.