# Compare pipelines

When the compiler pipeline produces unexpected output, use this workflow to
isolate the divergent pass. Since there is no longer a legacy pipeline to
compare against, the workflow focuses on comparing intermediate representations
at different stages.

## 1. Produce a minimal test program

Create a single `Main.coal` that exhibits the problem. Keep it as small
as possible — ideally one function.

## 2. Run the compiler

```bash
coal compile -I. Main.coal -o /tmp/test && /tmp/test
```

Capture the exact output, exit code, and any error messages.

## 3. Inspect kernel IR at each normalization stage

Use compiler flags to dump kernel IR at various stages:

```bash
coal compile --dump-kernel Main.coal
```

If the compiler supports per-pass dump flags, capture the output after each
normalization phase:

```
structuralNorm → functionalNorm → controlFlowNorm
```

## 4. Compare with known-good output

If a regression is suspected, use `git bisect` or compare against a known-good
build of the compiler. Focus on differences in:

- Variable names and bindings
- Constructor application arity
- Let-binding structure
- Closure representations

## 5. Compare the IR against expected invariants

Check the kernel IR against the documented invariants (`03-migration-policy.md`):

- The program is in ANF after `controlFlowNorm`
- All constructor applications are fully saturated
- Lambdas are flattened (no nested `fn(a) => fn(b) =>`)
- Local names are unique after `localNameCanonicalization`

## 6. Isolate the divergent pass

Once you identify which pass produces incorrect output, read the pass
implementation in `src/Coal/Kernel/Pipeline/Pass/`. Look for:

- Different traversal orders
- Different handling of edge cases (empty expressions, unit values, etc.)
- Missing cases in the implementation
- Incorrect freshness strategies for generated names

## 7. Verify the fix

After making a change, re-run the comparison to confirm the output is correct.
Then run the full test program to confirm end-to-end correctness.

Do **not** run `stack test` to verify; hand control back to the user or run a
single targeted test.