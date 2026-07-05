# Compare pipelines

When the new compiler pipeline produces unexpected output, compare its behaviour
against the legacy compiler to isolate the divergent pass.

## 1. Produce a minimal test program

Create a single `Main.coal` that exhibits the difference. Keep it as small
as possible — ideally one function.

## 2. Run both pipelines

**Legacy compiler** (uses `runtime/`):
```bash
coal compile --legacy -I. Main.coal -o /tmp/legacy && /tmp/legacy
```

**New compiler** (uses `runtime-next/`):
```bash
coal compile -I. Main.coal -o /tmp/new && /tmp/new
```

If the `--legacy` flag is not available, consult the CLI help (`coal --help`)
or check `app/Coal/CLI/Options/` for the correct flag.

## 3. Compare kernel IR

If both compilers can dump kernel IR, capture the output after each
normalization phase:

```bash
coal compile --dump-kernel Main.coal > new.ir
coal compile --legacy --dump-kernel Main.coal > legacy.ir
diff legacy.ir new.ir
```

Focus on differences in:
- Variable names and bindings
- Constructor application arity
- Let-binding structure
- Closure representations

## 4. If only one pipeline can be run at a time

If the legacy pipeline is not accessible via CLI, compare the source code of
the normalization passes directly:

- Legacy passes: `src/Coal/LegacyKernel/Compiler.hs`
- New passes: `src/Coal/Kernel/Pipeline/Pass/`

Identify which pass in the new pipeline corresponds to which step in the
legacy pipeline. The legacy pass sequence is:
```
astSortMatchClauses → astSuffix → astFlatten → astSaturateConstructors →
astLiftLambdaNodes → astSimplify1 → astSimplify2 → astCloseObjects → astAddExtraArgs
```

The new pass sequence is:
```
caseExpressionCanonicalization → localNameCanonicalization → lambdaFlattening →
constructorSaturation → lambdaLifting → topLevelFunctionNormalization →
functionResultsSaturation → logicalOperatorTranslation →
letBindingSimplification → administrativeNormalForm
```

## 5. Isolate the divergent pass

Once you identify which pass produces different output, focus on that pass
alone. Read both the legacy and new implementations. Look for:
- Different traversal orders
- Different handling of edge cases (empty expressions, unit values, etc.)
- Missing cases in the new implementation
- Different freshness strategies for generated names

## 6. Verify the fix

After making a change, re-run the comparison to confirm the outputs now match.
Then run the full test program to confirm end-to-end correctness.

Do **not** run `stack test` to verify; hand control back to the user or run a
single targeted test.